;; font
(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco" :height 100 :weight 'normal)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 100 :weight 'normal))