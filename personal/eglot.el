;;; eglot.el --- Eglot LSP configuration -*- lexical-binding: t; -*-

;; Eglot - built-in LSP client for Emacs 29+
;; Provides code completion, diagnostics, navigation, refactoring

(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)  ; shutdown server when last buffer closes
  (eglot-events-buffer-size 0)  ; don't store events
  :config
  ;; Use corfu for completion
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  
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
