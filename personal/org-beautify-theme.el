(deftheme org-beautify "Sub-theme to beautify org mode")

(defcustom org-beautify-theme-use-box-hack 't
  "Use a 3 pixel box with the background color to add spacing.
Note that this has a side effect that can make the theme look
really bad under some circumstances."
  :type 'boolean)

(let* ((sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
			((x-list-fonts "Verdana") '(:font "Verdana"))
			((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
			(nil (warn "Cannot find a Sans Serif Font.  Please report at: https://github.com/jonnay/org-beautify-theme/issues"))))
       (base-font-color (face-foreground 'default  nil 'default))
       (background-color (face-background 'default nil 'default))
       (headline `(:inherit default :foreground ,base-font-color))
       (primary-color (face-foreground 'mode-line nil))
       (secondary-color (face-background 'secondary-selection nil 'region))
       (padding (if org-beautify-theme-use-box-hack
		    `(:line-width 5 :color ,background-color)
		    nil))
       (org-highlights `(:foreground ,base-font-color :background ,secondary-color)))
  (custom-theme-set-faces 'org-beautify
			  `(org-agenda-structure ((t (:inherit default ,@sans-font :height 2.0 :underline nil))))
			  `(org-level-8 ((t ,headline)))
			  `(org-level-7 ((t ,headline)))
			  `(org-level-6 ((t ,headline)))
			  `(org-level-5 ((t ,headline)))
			  `(org-level-4 ((t ,headline)))
			  `(org-level-3 ((t (,@headline  :box ,padding))))
			  `(org-level-2 ((t (,@headline ,@sans-font :height 1.25 :box ,padding))))
			  `(org-level-1 ((t (,@headline ,@sans-font :height 1.5  :box ,padding ))))
			  `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box ,padding))))

			  `(org-block ((t (:foreground ,base-font-color :background ,background-color :box nil))))
			  `(org-block-begin-line ((t ,org-highlights)))
			  `(org-block-end-line ((t ,org-highlights)))

			  `(org-checkbox ((t (:foreground "#000000", :background "#93a1a1" :box (:line-width -3 :color "#93a1a1" :style "released-button")))))

			  `(org-headline-done ((t (:strike-through t))))
			  `(org-done ((t (:strike-through t))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'org-beautify)
;;; org-beautify-theme.el ends here
