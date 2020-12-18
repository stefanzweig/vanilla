;; For loading personal configurations
(defun personal (library)
  (load (concat vanilla-personal-dir "/" (symbol-name library)) 'noerror))

;; For loading packages from the Emacs Lisp Package Archive (ELPA)
(defun package (package)
    (when (not (package-installed-p package))
      (package-install package))
    (personal package))

;; For loading libraries from the vendor directory
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
