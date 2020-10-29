(defun jump-forward (phrase)
  "Move forward looking for PHRASE.  If not found, jump backward to it."
  (if (not (search-forward phrase nil t))
      (search-backward phrase nil t)))

(defun jump-backward (phrase)
  "Move backward looking for PHRASE.  If not found, jump foreward to it."
  (if (not (search-backward phrase nil t))
      (search-forward phrase nil t)))


(defun insert-typewriter (str)
  "Insert STR into the current buffer as if you were typing it by hand."
  (interactive "s")
  ;; (insert str)
  (dolist (ch (string-to-list str))
    (insert ch)
    (sit-for (/ 1.0 (+ 10 (random 100))) nil)))


;; the following copied from xah lee's configs.

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').
URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
	(kill-new (buffer-string))
	(delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
	       (kill-region (region-beginning) (region-end) t)
	     (kill-region (line-beginning-position) (line-beginning-position 2))))))


(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').
URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2018-09-10"
  (interactive)
  (if current-prefix-arg
      (progn
	(copy-region-as-kill (point-min) (point-max)))
    (if (use-region-p)
	(progn
	  (copy-region-as-kill (region-beginning) (region-end)))
      (if (eq last-command this-command)
	  (if (eobp)
	      (progn )
	    (progn
	      (kill-append "\n" nil)
	      (kill-append
	       (buffer-substring-no-properties (line-beginning-position) (line-end-position))
	       nil)
	      (progn
		(end-of-line)
		(forward-char))))
	(if (eobp)
	    (if (eq (char-before) 10 )
		(progn )
	      (progn
		(copy-region-as-kill (line-beginning-position) (line-end-position))
		(end-of-line)))
	  (progn
	    (copy-region-as-kill (line-beginning-position) (line-end-position))
	    (end-of-line)
	    (forward-char)))))))

(random t)

(defun xah-insert-random-hex (NUM)
  "Insert NUM random hexadecimal digits.
NUM default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-08-03"
  (interactive "P")
  (let (($n (if (numberp NUM) (abs NUM) 5 )))
    (insert (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n)))))))

(defun xah-insert-random-string (NUM)
  "Insert a random alphanumerics string of length 5.
The possible chars are digits and upper/lower case English letters but without any vowels nor Ll01.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2020-07-19"
  (interactive "P")
  (let* (($charset "bcdfghjkmnpqrstvwxyz23456789BCDFGHJKMNPQRSTVWXYZ")
	 ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount))))))

(defun xah-insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let (($charset "1234567890" )
	($baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5 ))
      (insert (elt $charset (random $baseCount))))))

(defun xah-insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
Version 2020-06-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((string-equal system-type "darwin") ; Mac
    (shell-command "uuidgen" t))
   ((string-equal system-type "gnu/linux")
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
			      (user-uid)
			      (emacs-pid)
			      (system-name)
			      (user-full-name)
			      (current-time)
			      (emacs-uptime)
			      (garbage-collect)
			      (buffer-string)
			      (random)
			      (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
		      (substring myStr 0 8)
		      (substring myStr 8 12)
		      (substring myStr 13 16)
		      (format "%x" (+ 8 (random 4)))
		      (substring myStr 17 20)
		      (substring myStr 20 32)))))))
