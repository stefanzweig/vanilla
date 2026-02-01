;;; Personal functions

;; For loading personal configurations
(defun personal (library)
  (load (concat vanilla-personal-dir "/" (symbol-name library)) 'noerror))

;; For loading packages from the Emacs Lisp Package Archive (ELPA)
(defun package (package)
    (when (not (package-installed-p package))
      (package-install package))
    (personal package))

;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
	 (normal (concat vanilla-vendor-dir "/" file))
	 (suffix (concat normal ".el"))
	 (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when found
      (if autoload-functions
	  (dolist (autoload-function autoload-functions)
	    (autoload autoload-function (symbol-name library) nil t))
	(require library)))
    (personal library)))

;; Arrows are common, especially in ruby
(defun insert-arrow ()
  (interactive)
  (delete-horizontal-space)
  (insert " => "))

(defun insert-file-name ()
  "Insert the full path file name into the current buffer."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; Quickly jump back and forth between matching parens/brackets
(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
	((looking-back "\\s)" 1) (backward-sexp arg))
	;; Now, try to succeed from inside of a bracket
	((looking-at "\\s)") (forward-char) (backward-sexp arg))
	((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; Make the whole buffer pretty and consistent
(defun iwb()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun delete-window-replacement (&optional p)
  "Kill current window.  If called with PREFIX, kill the buffer too."
  (interactive "P")
  (if p
      (kill-buffer nil))
  (delete-window))

(defun delete-other-windows-replacement (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (if p
      (dolist (window (window-list))
	(unless (equal (window-buffer window) (current-buffer))
	  (kill-buffer (window-buffer window)))))
  (delete-other-windows))

;; This override for transpose-words fixes what I consider to be a flaw with the
;; default implementation in simple.el. To transpose chars or lines, you always
;; put the point on the second char or line to transpose with the previous char
;; or line.  The default transpose-words implementation does the opposite by
;; flipping the current word with the next word instead of the previous word.
;; The new implementation below instead makes transpose-words more consistent
;; with how transpose-chars and transpose-lines behave.
(defun transpose-words (arg)
  "[Override for default transpose-words in simple.el]
Interchange words around point, leaving point at end of
them. With prefix arg ARG, effect is to take word before or
around point and drag it backward past ARG other words (forward
if ARG negative).  If ARG is zero, the words around or after
point and around or after mark are interchanged."
  (interactive "*p")
  (if (eolp) (forward-char -1))
  (transpose-subr 'backward-word arg)
  (forward-word (+ arg 1)))

;; Borrowed from https://gist.github.com/1415844
;; Also see http://emacsworld.blogspot.com/2011/12/moving-buffers-between-windows.html
(require 'cl-lib)
(defun rotate-left (l)
  (append  (cdr l) (list (car l))))

(defun rotate-windows ()
  (interactive)
  (let ((start-positions (rotate-left (mapcar 'window-start (window-list))))
	(buffers (rotate-left (mapcar 'window-buffer (window-list)))))
    (mapcar* (lambda (window buffer pos)
	       (set-window-buffer window buffer)
	       (set-window-start window pos))
	     (window-list)
	     buffers
	     start-positions)))

;; Borrowed from http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(linum-mode 1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Borrowed from http://superuser.com/q/603421/8424
(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
			    ("\x201D" . "\"")
			    ("\x2018" . "'")
			    ("\x2019" . "'"))
			  nil beg end))

(defun yank-and-replace-smart-quotes ()
  "Yank (paste) and replace smart quotes from the source with ascii quotes."
  (interactive)
  (yank)
  (replace-smart-quotes (mark) (point)))

(defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
	(replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; ============ Shortening of often used commands ============

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'lmlw 'list-matching-lines2)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)
(defalias 'sh 'shell)
(defalias 'ps 'powershell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

;; Elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

;; ============ Modern functions from ~/.emacs.d ============

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
	(set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Mono SC Nerd" 16)) ;; 11 13 17 19 23
	;; chinese font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "Sarasa Mono SC Nerd")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
	(+my/better-font))))

(defun org-todo-list-current-file (&optional arg)
  "Like `org-todo-list', but using only the current buffer's file."
  (interactive "P")
  (let ((org-agenda-files (list (buffer-file-name (current-buffer)))))
    (if (null (car org-agenda-files))
	(error "%s is not visiting a file" (buffer-name (current-buffer)))
      (org-todo-list arg))))

(defun now ()
  (interactive)
  (insert (current-time-string)))

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
	(set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Mono SC Nerd" 16)) ;; 11 13 17 19 23
	;; chinese font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "Sarasa Mono SC Nerd")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
	(+my/better-font))))

(defun org-todo-list-current-file (&optional arg)
  "Like `org-todo-list', but using only the current buffer's file."
  (interactive "P")
  (let ((org-agenda-files (list (buffer-file-name (current-buffer)))))
    (if (null (car org-agenda-files))
	(error "%s is not visiting a file" (buffer-name (current-buffer)))
      (org-todo-list arg))))

 (defun prelude-copy-file-name-to-clipboard ()
   "Copy the current buffer file name to the clipboard."
   (interactive)
   (let ((filename (if (equal major-mode 'dired-mode)
		       default-directory
		     (buffer-file-name))))
     (when filename
       (kill-new filename)
       (message "Copied buffer file name '%s' to the clipboard." filename))))

 (defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
	(replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun now ()
  (interactive)
  (insert (current-time-string)))
