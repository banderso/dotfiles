;;; +===========================================================+
;;; | Language Options                                          |
;;; +===========================================================+

(require 'google-c-style)
(add-hook 'after-init-hook
          (lambda ()
            (global-color-identifiers-mode)
            (add-to-list 'color-identifiers:modes-alist
                         '(yy-mode . (""
                                      "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                                      (nil font-lock-variable-name-face))))))

;;;
;;; YASnippet
;;;

;(require 'yasnippet)
;(yas-global-mode 1)

;;;
;;; Autoload
;;;
(autoload 'paredit-mode "paredit"   
  "Minor mode for pseudo-structurally editing Lisp code." t)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

;;; 
;;; Markdown
;;
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;;
;;; Hooks
;;;

(add-hook 'html-mode-hook
	  (lambda()
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode f)))

(add-hook 'csharp-mode-hook 
	  '(lambda ()
	     (setq-default compile-command "nant -nologo -q -e -find")))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (add-to-list 'c-cleanup-list 'comment-close-slash)
             ;; (setq truncate-lines t)
             ;; (auto-fill-mode)
             ;; (setq c-basic-offset 4)
             ;; (yas/minor-mode)
             ;; (linum-mode)
             (define-key c-mode-map [(control c) (c)] 'compile)))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'yy-mode)
	      (ggtags-mode 1))))

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (yas/minor-mode)))

(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (paredit-mode +1)
	     (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)
	     (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)))

(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (setq c-basic-offset 4
		  tab-width 4)))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

;;;
;;; Indentation Style
;;;

;;(setq c-default-style '((awk-mode . "awk")
;;						(other . "ellemtel")))

;;;
;;; Flymake Options
;;;

					; Python
;; (defun flymake-pylint-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list "epylint" (list local-file))))

					; JavaScript
					;(require 'flymake-js)

					; General
;; (defun my-flymake-next-error ()
;;   (interactive)
;;   (flymake-goto-next-error)
;;   (let ((err (get-char-property (point) 'help-echo)))
;;     (when err
;;       (message err))))

					;(custom-set-faces '(flymake-errline 
					;		    ((((class color))(:background "indian red")))))
					;(custom-set-faces '(flymake-warnline 
					;		    ((((class color))(:background "royal blue")))))

;;;
;;; C++
;;;


;;; 
;;; Python
;;;
(add-hook 'python-mode-hook
	  '(lambda ()
	     (flymake-mode 1)
	     (setq tab-width 4
		   py-indent-offset 4
		   indent-tabs-mode nil
		   py-smart-indentation t)))

;;;
;;; JavaScript
;;;
;;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;;(add-hook 'js2-mode-hook 
;;;	  '(lambda ()
;;;	     ;place the thing here
;;;		 ))

;;;
;;; ActionScript
;;;
(autoload 'actionscript-mode "actionscript-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;;; 
;;; C#
;;;
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C#" t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.build$" . sgml-mode))

;;;
;;; Clojure
;;;
					;(require 'clojure-mode)
					;(require 'clojure-auto)
					;(require 'clojure-paredit)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)

;;;
;;; Ruby
;;;

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))


;;;
;;; Go
;;;
					;(require 'go-mode-load)

;;;
;;; Io
;;;
					;(require 'io-mode)

;;;
;;; Scala
;;;
					;(require 'scala-mode-auto)

;;;
;;; Scheme
;;;

(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "gsi -:s,d")
