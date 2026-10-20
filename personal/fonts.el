;; font
;; Only set custom font for GUI, terminal uses system font
(when (display-graphic-p)
  (set-face-attribute 'default nil :family "0xProto Nerd Font" :height 160 :weight 'normal))
