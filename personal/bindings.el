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

;; Help teach to unlearn the arrow keys
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f9> a") 'org-agenda)
(global-set-key (kbd "<f9> c") 'org-capture)
(global-set-key (kbd "<f9> m") 'my/yank-more)
(global-set-key (kbd "<f9> n") 'org-todo-list-current-file)
;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-s") 'swiper)

;; key-chord
;; (key-chord-define-global "jj" 'avy-goto-word-1)
;; (key-chord-define-global "jl" 'avy-goto-line)
;; (key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'xah-copy-line-or-region)
(key-chord-define-global "dd" 'xah-cut-line-or-region)

;; helm
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-c f") 'helm-recentf)

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
    ("q" nil "quit")))

(global-set-key (kbd "<f5> g") 'counsel-git-grep)
(global-set-key (kbd "<f6>") 'xah-select-block)
(global-set-key (kbd "<f7>") 'zweig/default_orgmode_playground)
(global-set-key (kbd "<f8>") 'xah-search-current-word)
