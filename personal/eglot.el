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
  (eglot-autoshutdown t)            ; shutdown server when last buffer closes
  (eglot-events-buffer-size 0)      ; don't store events
  :config
  ;; `eglot-mode-map' and `eglot-server-programs' are defined by eglot.
  ;; Touch them only once eglot is actually loaded — otherwise a bare
  ;; `:bind :map' or `add-to-list' trips a void-variable in batch /
  ;; deferred-load contexts.
  (with-eval-after-load 'eglot
    (when (boundp 'eglot-mode-map)
      (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
      (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
      (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
      (define-key eglot-mode-map (kbd "C-c l d") #'eglot-find-definition)
      (define-key eglot-mode-map (kbd "C-c l R") #'eglot-find-references)
      (define-key eglot-mode-map (kbd "C-c l h") #'eldoc))
    (when (boundp 'eglot-server-programs)
      (dolist (entry '((python-mode . ("pyright-langserver" "--stdio"))
                       ((c-mode c++-mode) . ("clangd"))
                       ((js-mode typescript-mode tsx-ts-mode)
                        . ("typescript-language-server" "--stdio"))))
        (add-to-list 'eglot-server-programs entry)))))

(provide 'eglot)
;;; eglot.el ends here
