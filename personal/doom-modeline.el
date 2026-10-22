;;; doom-modeline.el --- Doom Modeline configuration -*- lexical-binding: t; -*-

;; Doom Modeline - A fancy and fast mode-line
;; Replaces powerline

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-number-limit 999)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-lsp t)
  (doom-modeline-github-timer nil)
  (doom-modeline-env-version t)
  (doom-modeline-python-executable "python3"))

(provide 'vanilla-doom-modeline)
;;; doom-modeline.el ends here
