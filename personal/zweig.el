;; Code

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

(add-hook 'after-init-hook #'my/frame-recenter)
(add-hook 'after-make-frame-functions #'my/frame-recenter)


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
  (org-mode)
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

(provide 'zweig)
;;; zweig.el ends here
