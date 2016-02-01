(setq org-directory "~/org/")
(setq org-default-notes-file "~/.notes")

(setq org-capture-templates 
      '(("t" "Todo" entry (file+headline "~/org/journal.org" "Tasks") "* TODO %? %^g\n %i\n ")
        ("n" "Note" entry (file+headline "~/org/journal.org" "Notes") "* %? %^g\n %i\n ")
        ("l" "Log"  entry (file+headline "~/org/journal.org" "Log")   "* %T - %? %^g\n %i\n " )
        ("s" "Story" entry (file+headline "~/org/kanban.org" "Stories") "** [#%^{priority|A|B|C|D}] %? %^g\n %i\n")))

(setq org-log-done t)
(setq org-agenda-files '("~/org/journal.org"))
