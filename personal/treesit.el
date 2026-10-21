;;; treesit.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;; Tree-sitter for better syntax highlighting and navigation
;; Requires Emacs 29+

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :custom
  (treesit-font-lock-level 4)  ; maximum syntax highlighting
  :config
  ;; Auto-install missing grammars
  (when (fboundp 'treesit-auto-install-all)
    (treesit-auto-install-all)))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'treesit)
;;; treesit.el ends here
