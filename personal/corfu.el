;;; corfu.el --- Corfu configuration -*- lexical-binding: t; -*-

;; Corfu - Completion Overlay Region FUnction
;; Replaces company-mode

(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary t)     ; Quit at completion boundary
  (corfu-quit-no-match t)        ; Quit when there is no match
  (corfu-preview-current nil)    ; Disable current candidate preview
  (corfu-preselect 'prompt)      ; Preselect the prompt
  (corfu-on-exact-match nil)     ; Configure auto completion on exact match

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))

  :init
  (global-corfu-mode))

;; Popup completion info (built into corfu 1.0+ as corfu-popupinfo-mode)
(use-package corfu
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (corfu-popupinfo-mode 1))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'corfu)
;;; corfu.el ends here
