;;; eglot.el --- Eglot LSP configuration -*- lexical-binding: t; -*-

;; Eglot - built-in LSP client for Emacs 29+
;; Provides code completion, diagnostics, navigation, refactoring

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)  ; shutdown server when last buffer closes
  (eglot-events-buffer-size 0)  ; don't store events
  :config
  ;; Use corfu for completion
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  
  ;; Keybindings
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eglot-find-definition)
              ("C-c l R" . eglot-find-references)
              ("C-c l h" . eldoc)))

(provide 'vanilla-eglot)
;;; eglot.el ends here
