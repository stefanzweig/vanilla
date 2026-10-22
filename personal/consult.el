;;; consult.el --- Consult configuration -*- lexical-binding: t; -*-

;; Consult - consulting completing-read
;; Replaces swiper and counsel

(use-package consult
  :bind
  ;; C-x bindings in `ctl-x-map'
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x r x" . consult-register)
  ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ;; M-g bindings in `goto-map'
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-project-imenu)
  ;; M-s bindings in `search-map'
  ("M-s f" . consult-find)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-widen-key ">")
  :config
  ;; Configure consult-imenu for project support
  (consult-customize
   consult-theme :preview-key
   '(:debounce 0.2 "C-n" "C-p" "<down>" "<up>")))

;; Optionally configure the register formatting
(use-package emacs
  :bind
  ("C-x r p" . consult-register-load)
  ("C-x r w" . consult-register-store))

(provide 'vanilla-consult)
;;; consult.el ends here
