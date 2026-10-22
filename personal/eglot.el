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

  ;; TypeScript: typescript-language-server does NOT search the global npm
  ;; prefix -- it only looks at the workspace's node_modules/typescript or an
  ;; explicit tsserver path.  TypeScript 7+ (the Go port) ships no tsserver.js
  ;; at all, so we pin a TS 5.x install and expose it as `fallbackPath'.
  ;; fallbackPath (not path) is checked *after* the workspace version, so a
  ;; project's own TypeScript still takes priority.
  (let ((ts-server "typescript-language-server")
        (ts-tsserver "~/.local/share/eglot-typescript/node_modules/typescript/lib/tsserver.js"))
    (setq eglot-server-programs
          (cons `((js-ts-mode typescript-ts-mode tsx-ts-mode)
                  ,ts-server "--stdio"
                  :initializationOptions
                  ( :tsserver (:fallbackPath ,(expand-file-name ts-tsserver))
                    :maxTsServerMemory 3072))
                ;; drop any pre-existing TypeScript entry so we stay idempotent
                (seq-remove
                 (lambda (entry)
                   (let ((modes (car entry)))
                     (or (eq modes 'typescript-ts-mode)
                         (and (listp modes) (memq 'typescript-ts-mode modes)))))
                 eglot-server-programs))))
  
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
