(when (not (boundp 'org-remember-insinuate))
  (require 'org-remember))
(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file "~/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates 
      '(("Todo" ?t "* TODO %? %^g\n %i\n " "~/org/journal.org" "Tasks")
		("Note" ?n "* %? %^g\n %i\n " "~/org/journal.org" "Notes")
		("Log" ?l "* %T - %? %^g\n %i\n " "~/org/journal.org" "Log")))

(setq org-log-done t)
(setq org-agenda-files '("~/org/journal.org"))
