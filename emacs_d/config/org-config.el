
(setq org-directory "~/org/")
(setq org-default-notes-file "~/.notes")

(setq org-capture-templates 
      '(("Todo" ?t "* TODO %? %^g\n %i\n " "~/org/journal.org" "Tasks")
	("Note" ?n "* %? %^g\n %i\n " "~/org/journal.org" "Notes")
	("Log" ?l "* %T - %? %^g\n %i\n " "~/org/journal.org" "Log")))

(setq org-log-done t)
(setq org-agenda-files '("~/org/journal.org"))
