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

(provide 'zweig)
;;; zweig.el ends here
