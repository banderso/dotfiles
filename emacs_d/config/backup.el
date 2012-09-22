;;; +===========================================================+
;;; | Backup/Autosave Options                                   |
;;; +===========================================================+

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/tmp"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-version 2
      version-control t)
