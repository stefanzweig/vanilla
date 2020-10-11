(message "Vanilla is powering up... Be patient, Master!")

;; Define Vanilla's directory structure
(defvar vanilla-dir (file-name-directory load-file-name))

;; This directory is for your personal configuration.
(defvar vanilla-personal-dir (expand-file-name "personal" vanilla-dir))

;; This directory is for your vendor configuration.
(defvar vanilla-vendor-dir (expand-file-name "vendor" vanilla-dir))

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

;; elpa managed
;; ------------------
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(package 'magit)

;; main settings
(personal 'global)
(personal 'fonts)
(personal 'org)
(personal 'bindings)

;; submodule managed
;; ------------------
(add-to-list 'load-path (concat vanilla-vendor-dir "/"))
(message (concat vanilla-vendor-dir "/"))