(message "Vanilla is powering up... Be patient, Master!")

;; Define Vanilla's directory structure
(defvar vanilla-dir (file-name-directory load-file-name))

;; This directory is for your personal configuration.
(defvar vanilla-personal-dir (expand-file-name "personal" vanilla-dir))

;; This directory is for your vendor configuration.
(defvar vanilla-vendor-dir (expand-file-name "vendor" vanilla-dir))

;; This directory is for your backups configuration.
(defvar vanilla-backups-dir (expand-file-name "backups" vanilla-dir))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" vanilla-personal-dir))
(load custom-file 'noerror)

(setq def-init-file (expand-file-name "defuns.el" vanilla-personal-dir))
(load def-init-file 'noerror)

;; plugins
(personal 'crux)
(personal 'org-bullets)
(personal 'infrastructure)
(personal 'which-key)
(personal 'iedit)
(personal 'projectile)
(personal 'zweig)
(personal 'key-chord)
(personal 'winpulse)
;;(personal 'init-f2)
;; (personal 'burly)

;; elpa managed
;; ------------------
;; Pin package-user-dir to this repo's elpa/ so Emacs always loads from
;; (and installs into) the project directory — avoids the HOME-vs-repo
;; elpa split where some packages live in only one of the two trees.
(setq package-user-dir (expand-file-name "elpa" vanilla-dir))
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			 ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(package 'use-package)
(package 'magit)
(package 'expand-region)
;; (package 'key-chord)
;; (package 'avy)
(package 'vertico)      ; replaces ivy
(package 'consult)      ; replaces swiper + counsel
(package 'embark)       ; context-sensitive actions
(package 'orderless)    ; fuzzy matching for vertico
(package 'corfu)        ; replaces company
(package 'cape)          ; corfu completion-at-point extensions
(package 'embark-consult) ; embark + consult integration
;; (package 'doom-modeline) ; replaces powerline
(package 'treesit-auto)  ; auto-install tree-sitter grammars
;; eglot is built-in for Emacs 29+, no package needed
(package 'smartparens)
(package 'paredit)
;; (package 'demo-it)
(package 'yasnippet)
(package 'exec-path-from-shell)
(package 'highlight-parentheses)
;; (package 'markdown-mode)
;; (package 'undo-tree)
(package 'diminish)
;; (package 'solarized-theme)
(package 'hydra)
;; (package 'dashboard)
;; (package 'simple-httpd)

;; submodule managed
;; ------------------
(add-to-list 'load-path (concat vanilla-vendor-dir "/"))

;; main settings
(personal 'global)
(personal 'fonts)
;; (personal 'org-brain)
(personal 'org)
(personal 'vertico)
(personal 'consult)
(personal 'embark)
(personal 'corfu)
;; (personal 'doom-modeline)
(personal 'treesit)
(personal 'eglot)
(personal 'vanilla-utils)
(personal 'bindings)
;; (personal 'dashboard)
;; (+my/better-font)

(put 'narrow-to-region 'disabled nil)
;; org-babel-python-command lives in personal/org.el
;; (setq initial-major-mode (quote text-mode))
(setq initial-buffer-choice 'xah-new-empty-buffer)
