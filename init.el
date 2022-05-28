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
(personal 'init-f2)
(personal 'zweig)
;; (personal 'burly)

;; elpa managed
;; ------------------
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

;; (package 'use-package)
(package 'magit)
(package 'expand-region)
(package 'key-chord)
(package 'avy)
(package 'company)
(package 'helm)
(package 'helm-projectile)
(package 'ivy)
(package 'swiper)
(package 'counsel)
(package 'smartparens)
(package 'paredit)
;; (package 'demo-it)
(package 'yasnippet)
(package 'powerline)
(package 'exec-path-from-shell)
(package 'highlight-parentheses)
;; (package 'markdown-mode)
;; (package 'undo-tree)
(package 'diminish)
;; (package 'solarized-theme)
;; (package 'ivy-posframe)
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
(personal 'vanilla-company)
(personal 'vanilla-utils)
(personal 'bindings)
;; (personal 'dashboard)
;; (+my/better-font)

(put 'narrow-to-region 'disabled nil)
(setq org-babel-python-command "python3")
;; (setq initial-major-mode (quote text-mode))
(setq initial-buffer-choice 'xah-new-empty-buffer)
