;; font
;; Only set custom font for GUI, terminal uses system font
(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Iosevka" :height 220 :weight 'normal))
