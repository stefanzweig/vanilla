;;; vertico.el --- Vertico configuration -*- lexical-binding: t; -*-

;; Vertico - vertical interactive completion UI
;; Replaces ivy

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ; cycle from bottom to top
  :config
  ;; Save history
  (savehist-mode)
  (setq history-length 25)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; Use orderless for fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Better minibuffer experience
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (if (vertico--advice-p)
                    (cons (format "%s%s "
                                  (car args)
                                  crm-separator)
                          (cdr args))
                  args)))
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(provide 'vanilla-vertico)
;;; vertico.el ends here
