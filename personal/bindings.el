;;
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window
(global-unset-key (kbd "<f11>")) ; was fullscreen
(global-unset-key (kbd "C-SPC")) ; was mark

;; Help teach to unlearn the arrow keys
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f8> a") 'org-agenda)
(global-set-key (kbd "<f8> c") 'org-capture)
(global-set-key (kbd "<f8> m") 'my/yank-more)
(global-set-key (kbd "<f8> l") 'org-todo-list-current-file)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
;; (global-set-key (kbd "M-p") 'previous-buffer)
;; (global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-,") #'crux-duplicate-current-line-or-region)
;;(global-set-key (kbd "C-m") #'eval-print-last-sexp)

;; key-chord
(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "kk" 'execute-extended-command)
(key-chord-define-global "yy" 'xah-copy-line-or-region)
(key-chord-define-global "dd" 'xah-cut-line-or-region)
(key-chord-define-global "hh" 'swiper-thing-at-point)
(key-chord-define-global "gg" 'counsel-git-grep)

;; helm
;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;;(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-m") 'helm-M-x)
;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "C-h f") 'helm-apropos)
;;(global-set-key (kbd "C-h r") 'helm-info-emacs)
;;(global-set-key (kbd "C-h C-l") 'helm-locate-library)
;;(global-set-key (kbd "C-c f") 'helm-recentf)

;; shell related
(global-set-key (kbd "C-!") 'shell)

;; xah cut/copy
;; (global-set-key (kbd "<f7>") 'xah-cut-line-or-region) ; cut
;; (global-set-key (kbd "<f8>") 'xah-copy-line-or-region) ; copy

(when (bound-and-true-p hydra-verbatim)
  (defhydra hydra-utils (global-map "C-t")
    "hydra"
    ("j" next-line "down" :column "Vertical")
    ("k" previous-line "up")
    ("l" forward-char "forward" :column "Horizontal")
    ("h" backward-char "back")
    ("+" text-scale-increase "in" :column "Zoom")
    ("-" text-scale-decrease "out" :column "Zoom")
    ("g" counsel-git-grep "grep" :column "Utils")
    ("n" next-buffer "next buffer" :column "Utils")
    ("p" previous-buffer "previous buffer" :column "Utils")
    ("q" nil "quit" :column "Opts")))

(global-set-key (kbd "<f8> (") 'xah-insert-paren)
(global-set-key (kbd "<f8> <f8>") 'xah-search-current-word)
(global-set-key (kbd "<f8> [") 'xah-insert-square-bracket)
(global-set-key (kbd "<f8> f") 'prelude-copy-file-name-to-clipboard)
(global-set-key (kbd "<f8> g") 'counsel-git-grep)
(global-set-key (kbd "<f8> n") 'zweig/default_orgmode_playground)
(global-set-key (kbd "<f8> o") 'xah-open-file-at-cursor)
(global-set-key (kbd "<f8> s") 'xah-select-block)
(global-set-key (kbd "<f8> {") 'xah-insert-brace)

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; mc
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hydra

(defhydra hydra-zoom (global-map "<f5>")
  "HYDRA"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("n" next-buffer "next buffer")
  ("p" previous-buffer "previous buffer"))

(defhydra hydra-org-clock (:exit t :color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
	_i_n         _e_dit   _g_oto entry
	_c_ontinue   _q_uit   _d_isplay
	_o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))
