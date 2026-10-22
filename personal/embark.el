;;; embark.el --- Embark configuration -*- lexical-binding: t; -*-

;; Embark - Emacs Mini-Buffer Actions Rooted in Keymaps
;; Provides context-sensitive actions

(use-package embark
  :bind
  (("C-." . embark-act)         ; pick some comfortable binding
   ("C-;" . embark-dwim)        ; good alternative: M-.
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at the via echo messages
  (add-hook 'embark-collect-mode-hook #'embark-indicators-minibuffer)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
;; Defer until both embark and consult are actually loaded — without
;; :demand, which forces an early require that runs before embark's
;; variables (e.g. `embark-transformer-alist') are defined.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'embark)
;;; embark.el ends here
