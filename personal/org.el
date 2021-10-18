(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(setq org-expiry-inactive-timestamps t)
(setq org-clock-idle-time nil)
(setq org-log-done 'time)
(setq org-clock-auto-clock-resolution nil)
(setq org-clock-continuously nil)
(setq org-clock-persist t)
(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-in-resume nil)
(setq org-show-notification-handler 'message)
(setq org-clock-report-include-clocking-task t)
(org-clock-persistence-insinuate)

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

(setq org-ditaa-jar-path (concat vanilla-personal-dir "/" "ditaa.jar"))

(setq org-plantuml-jar-path (concat vanilla-personal-dir "/" "plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(require 'ob)
(require 'ob-shell)
(require 'ob-python)
(require 'ob-tangle)
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages   '((python . t)
			      (emacs-lisp . t)
			      (shell . t)
			      (java . t)
			      (C . t)
			      (calc . t)
			      (dot . t)
			      (ditaa . t)
			      (plantuml . t)))
(defun org-babel-execute:yaml (body params) body)

(setq org-agenda-files (quote ("~/Documents/2020" "~/Documents/2021")))
(setq org-default-notes-file "~/Documents/2020/notes.org")

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Documents/2020/newgtd.org")
	 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	("T" "Quick task" entry (file "~/Documents/2020/newgtd.org")
	 "* TODO %^{Task}\n" :immediate-finish t)
	("j" "Journal" entry (file+datetree "~/Documents/2020/journal.org")
	 "** %^{Heading}")))

;; org mode agenda customized commands
(setq org-agenda-custom-commands
      '(("D" "Daily Action List"
	 ((agenda "" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up))))
		      (org-deadline-warning-days 0)))))
	("P" "By priority"
	 ((tags-todo "+PRIORITY=\"A\"")
	  (tags-todo "+PRIORITY=\"B\"")
	  (tags-todo "+PRIORITY=\"\"")
	  (tags-todo "+PRIORITY=\"C\""))
	 ((org-agenda-prefix-format "%-10c %-10T %e ")
	  (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
	))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAIT(w@/!)" "APPT(a@/!)" "|" "CNCL(c@/!)" "DFRD(f@/)"))))

;; Hide leading stars
(setq org-hide-leading-stars t)

(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))


(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function my/checkbox-list-complete)))

(defun my/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (when (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
			     (line-end-position) t)
      (org-todo (if (if (match-end 1)
			(equal (match-string 1) "100%")
		      (and (> (match-end 2) (match-beginning 2))
			   (equal (match-string 2) (match-string 3))))
		    ;; All done - do the state change.
		    'done
		  'todo)))))


(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
