;;; +===========================================================+
;;; | Slime options                                             |
;;; +===========================================================+

;(eval-after-load "slime"
;  '(progn (slime-setup '(slime-repl))))

;(setq inferior-lisp-program "/usr/bin/sbcl")

; SBCL
;(setq slime-lisp-implementations '((sbcl ("sbcl"))))

; Clojure
;; (setq swank-clojure-jar-path "/usr/local/Cellar/clojure/1.2.1/clojure.jar"
;;       swank-clojure-extra-classpaths "/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar")

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook
		  (defun clojure-mode-slime-font-lock ()
			(let (font-lock-mode)
			  (clojure-mode-font-lock-setup))))
				      
;(require 'swank-clojure)
;(require 'slime)
;(slime-setup)

