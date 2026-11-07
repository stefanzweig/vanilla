;;; fonts.el --- GUI fonts picked so that CJK is exactly two Latin columns

;; Why this file is fussy about advance widths:
;;
;; Emacs lays a CJK character out over two Latin columns, so org tables (and
;; anything else driven by `string-width') only line up when the CJK glyph is
;; drawn exactly twice as wide as one Latin cell.  A Latin cell is the advance
;; of a space, so what matters is space-width vs han-width:
;;
;;   Fira Code    0.615 em  ->  2 x 0.615 = 1.230 em is allocated per han
;;                              character, but the CJK fallback (PingFang)
;;                              still draws 1.000 em.  Every han character
;;                              falls 18.8% short, so the `|' separators
;;                              drift right ~1.5 cells per four characters.
;;   Sarasa Mono  0.500 em  ->  2 x 0.500 = 1.000 em = exactly the CJK
;;                              advance.  No rescaling, no drift.
;;
;; Hence: only 0.5 em families belong here.  Adding a 0.6 em font such as
;; Fira Code or JetBrains Mono silently breaks CJK alignment again.
;; Note the file itself is always correct -- `org-table-align' counts 2
;; columns per han character -- it is purely the rendering that drifts, so
;; realigning in Emacs cannot fix it.

(defconst my/gui-fonts
  '(("Sarasa Mono SC" . 160)   ; Iosevka-based Latin + Source Han CJK, 2:1
    ("Iosevka"        . 160))  ; 2:1 with any 1.0 em CJK font
  "Preferred GUI fonts, first installed one wins.
Both have a 0.5 em advance, which is what keeps CJK at exactly two columns.")

(defconst my/nerd-icon-font "Symbols Nerd Font"
  "Fallback for the private use glyphs used by powerline separators.")

;; Only set custom font for GUI, terminal uses system font
(when (display-graphic-p)
  (let ((choice (seq-find (lambda (c) (find-font (font-spec :family (car c))))
                          my/gui-fonts)))
    (when choice
      (let ((family (car choice))
            (height (cdr choice)))
        (set-face-attribute 'default nil
                            :family family :height height :weight 'normal)
        ;; Pin CJK to the same family.  Without this it silently falls back to
        ;; PingFang, which is 1.0 em -- fine on its own, but it hides the fact
        ;; that the Latin side changed width, so the 2:1 invariant is lost.
        (dolist (charset '(han cjk-misc bopomofo kana))
          (set-fontset-font t charset (font-spec :family family)))
        ;; Sarasa is not Nerd-patched: keep the powerline separators working.
        (when (find-font (font-spec :family my/nerd-icon-font))
          (set-fontset-font t '(#xe000 . #xf8ff)
                            (font-spec :family my/nerd-icon-font) nil 'append))))))
