;; font
(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 120 :weight 'normal)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 120 :weight 'normal))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))
