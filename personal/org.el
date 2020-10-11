(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(require 'ob)
(require 'ob-shell)
(require 'ob-ruby)
(require 'ob-python)
(require 'ob-sass)
(require 'ob-tangle)
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-language   '((python . t)
                              (emacs-lisp . t)
                              (shell . t)
                              (ruby . t)
                              (sass . t)
                              (dot . t)
                              (java . t)
                              (ditaa . t)
                              (plantuml . t)))
(defun org-babel-execute:yaml (body params) body)

(setq org-agenda-files (quote ("~/Documents/2020")))
(setq org-default-notes-file "~/Documents/2020/notes.org")

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Documents/2020/newgtd.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
      ("j" "Journal" entry (file+datetree "~/Documents/2020/journal.org")
         "** %^{Heading}")))

;; org mode agenda customized commands
(setq org-agenda-custom-commands
      '(("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up))))
                      (org-deadline-warning-days 0)))))))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAIT(w@/!)" "APPT(a@/!)" "|" "CNCL(c@/!)" "DFRD(f@/)"))))

;; Hide leading stars
(setq org-hide-leading-stars t)