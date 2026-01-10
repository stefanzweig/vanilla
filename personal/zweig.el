;; Code

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
	    (setq c-basic-offset 4)))

(defun zweig/showinfo ()
    (interactive)
  (insert (now)))

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
			    frame)
		      (selected-frame)))
	   (frame-w (frame-pixel-width frame))
	   (frame-h (frame-pixel-height frame))
	   ;; frame-monitor-workarea returns (x y width height) for the monitor
	   (monitor-w (nth 2 (frame-monitor-workarea frame)))
	   (monitor-h (nth 3 (frame-monitor-workarea frame)))
	   (center (list (/ (- monitor-w frame-w) 2)
			 (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

;; (add-hook 'after-init-hook #'my/frame-recenter)
;; (add-hook 'after-make-frame-functions #'my/frame-recenter)


(defun zweig/python_playground ()
  (interactive)
  (crux-create-scratch-buffer)
  (python-mode)
  )

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun zweig/default_orgmode_playground ()
  (interactive)
  (crux-create-scratch-buffer)
  ;;(org-mode)
  (text-mode)
  )

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
	 ($inputStr
	  (if (use-region-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (let ($p0 $p1 $p2
		      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
		      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
	      (setq $p0 (point))
	      (skip-chars-backward $pathStops)
	      (setq $p1 (point))
	      (goto-char $p0)
	      (skip-chars-forward $pathStops)
	      (setq $p2 (point))
	      (goto-char $p0)
	      (buffer-substring-no-properties $p1 $p2))))
	 ($path
	  (replace-regexp-in-string
	   "^file:///" "/"
	   (replace-regexp-in-string
	    ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
	(if (fboundp 'xahsite-url-to-filepath)
	    (let (($x (xahsite-url-to-filepath $path)))
	      (if (string-match "^http" $x )
		  (browse-url $x)
		(find-file $x)))
	  (progn (browse-url $path)))
      (progn ; not starting “http://”
	(if (string-match "#" $path )
	    (let (
		  ( $fpath (substring $path 0 (match-beginning 0)))
		  ( $fractPart (substring $path (1+ (match-beginning 0)))))
	      (if (file-exists-p $fpath)
		  (progn
		    (find-file $fpath)
		    (goto-char (point-min))
		    (search-forward $fractPart ))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		  (find-file $fpath))))
	  (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
	      (let (
		    ($fpath (match-string 1 $path))
		    ($line-num (string-to-number (match-string 2 $path))))
		(if (file-exists-p $fpath)
		    (progn
		      (find-file $fpath)
		      (goto-char (point-min))
		      (forward-line (1- $line-num)))
		  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		    (find-file $fpath))))
	    (if (file-exists-p $path)
		(progn ; open f.ts instead of f.js
		  (let (($ext (file-name-extension $path))
			($fnamecore (file-name-sans-extension $path)))
		    (if (and (string-equal $ext "js")
			     (file-exists-p (concat $fnamecore ".ts")))
			(find-file (concat $fnamecore ".ts"))
		      (find-file $path))))
	      (if (file-exists-p (concat $path ".el"))
		  (find-file (concat $path ".el"))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
		  (find-file $path ))))))))))

(defun xah-insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 LBracket and RBracket are strings. WrapMethod must be either `line' or `block'. `block' means between empty lines.

• if there is a region, add brackets around region.
• If WrapMethod is `line', wrap around line.
• If WrapMethod is `block', wrap around block.
• if cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html'
Version: 2017-01-17 2021-08-12"
  (if (region-active-p)
      (progn
	(let ( ($p1 (region-beginning)) ($p2 (region-end)))
	  (goto-char $p2) (insert RBracket)
	  (goto-char $p1) (insert LBracket)
	  (goto-char (+ $p2 2))))
    (let ($p1 $p2)
      (cond
       ((eq WrapMethod 'line)
	(setq $p1 (line-beginning-position) $p2 (line-end-position))
	(goto-char $p2)
	(insert RBracket)
	(goto-char $p1)
	(insert LBracket)
	(goto-char (+ $p2 (length LBracket))))
       ((eq WrapMethod 'block)
	(save-excursion
	  (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
	  (goto-char $p2)
	  (insert RBracket)
	  (goto-char $p1)
	  (insert LBracket)
	  (goto-char (+ $p2 (length LBracket)))))
       ( ;  do line. line must contain space
	(and
	 (eq (point) (line-beginning-position))
	 ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (not (eq (line-beginning-position) (line-end-position))))
	(insert LBracket )
	(end-of-line)
	(insert  RBracket))
       ((and
	 (or ; cursor is at end of word or buffer. i.e. xyz▮
	  (looking-at "[^-_[:alnum:]]")
	  (eq (point) (point-max)))
	 (not (or
	       (string-equal major-mode "xah-elisp-mode")
	       (string-equal major-mode "emacs-lisp-mode")
	       (string-equal major-mode "lisp-mode")
	       (string-equal major-mode "lisp-interaction-mode")
	       (string-equal major-mode "common-lisp-mode")
	       (string-equal major-mode "clojure-mode")
	       (string-equal major-mode "xah-clojure-mode")
	       (string-equal major-mode "scheme-mode"))))
	(progn
	  (setq $p1 (point) $p2 (point))
	  (insert LBracket RBracket)
	  (search-backward RBracket )))
       (t (progn
	    ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
	    ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
	    (skip-chars-backward "-_[:alnum:]")
	    (setq $p1 (point))
	    (skip-chars-forward "-_[:alnum:]")
	    (setq $p2 (point))
	    (goto-char $p2)
	    (insert RBracket)
	    (goto-char $p1)
	    (insert LBracket)
	    (goto-char (+ $p2 (length LBracket)))))))))

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )

;; move line up down
;; 2024-11-25 10:12:45
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg))
	(forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [S-C-up] 'move-text-up)
(global-set-key [S-C-down] 'move-text-down)

(defun toggle-relative-ln ()
  (interactive)
  (if (and (boundp 'display-line-numbers-mode) display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (progn
      (setq display-line-numbers-type 'relative)
      (display-line-numbers-mode 1))))

;;
(defun split-region-by-two-chars-safe (beg end)
  "安全地对选中的区域按每两个字符进行分段。"
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (line-beginning-position) (line-end-position))))

  (let ((text (buffer-substring-no-properties beg end))
	(separator " "))
    (save-excursion
      (delete-region beg end)
      (insert (replace-regexp-in-string "\\(..\\)"
					(concat "\\1" separator) text)))))

;;
(defun split-region-by-four-chars-safe (beg end)
  "安全地对选中的区域按每两个字符进行分段。"
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (line-beginning-position) (line-end-position))))

  (let ((text (buffer-substring-no-properties beg end))
	(separator " "))
    (save-excursion
      (delete-region beg end)
      (insert (replace-regexp-in-string "\\(....\\)"
					(concat "\\1" separator) text)))))

;;
(defun convert-selected-hex-to-decimal (beg end)
  "将选中的十六进制文本转换为十进制。"
  (interactive "r")
  (when (use-region-p)
    (let* ((hex-str (buffer-substring-no-properties beg end))
	   (decimal (string-to-number hex-str 16)))
      (delete-region beg end)
      (insert (number-to-string decimal)))))

(provide 'zweig)
;;; zweig.el ends here
