;;; +===========================================================+
;;; | Custom Commands                                           |
;;; +===========================================================+

;;; use C-x C-m or C-c C-m instead of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;; use C-w for "backspace" and move kill-region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; Flymake
(global-set-key "\C-c\C-z" 'my-flymake-next-error)

; Org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-capture)

;(define-key global-map [?\C- ] 'set-mark-command)

(define-key global-map "\C-ct" 'insert-date-time-stamp)
(define-key global-map "\C-c\C-t" 'insert-date-time-stamp)

(define-key global-map "\C-cd" 'dash-at-point)
