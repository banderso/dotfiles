;;; +===========================================================+
;;; | Language Options                                          |
;;; +===========================================================+

(require 'google-c-style)
(add-hook 'after-init-hook
          '(lambda ()
            (add-to-list 'color-identifiers:modes-alist
                         '(yy-mode . (""
                                      "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                                      (nil font-lock-variable-name-face))))))

;;;
;;; YASnippet
;;;

;(require 'yasnippet)
                                        ;(yas-global-mode 1)
;(yas-reload-all)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;;;
;;; Autoload
;;;
(autoload 'paredit-mode "paredit"   
  "Minor mode for pseudo-structurally editing Lisp code." t)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq markdown-command "mmd")

;;; 
;;; Markdown
;;
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;;
;;; Hooks
;;;

(add-hook 'html-mode-hook
	  '(lambda()
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode nil)))

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
	  '(lambda ()
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

(add-hook 'lisp-mode-hook '(lambda () (paredit-mode +1)))

(add-hook 'java-mode-hook
	  '(lambda ()
	    (setq c-basic-offset 4
              tab-width 4)))


;;;
;;; Rust
;;;

(setq rust-format-on-save t)
(setq rust-indent-offset 2)
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  (require 'lsp-rust))

(add-hook 'rust-mode-hook #'color-identifiers-mode)
(add-hook 'rust-mode-hook #'lsp-rust-enable)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(add-hook 'company-mode
          '(lambda ()
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;;;
;;; Language Server Protocol
;;;

(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (require 'company-lsp)
  (push 'company-lsp company-backends))
(require 'lsp-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook #'cargo-minor-mode)
;; (add-hook 'rust-mode-hook #'flycheck-mode)
;; (add-hook 'rust-mode-hook
;;           '(lambda ()
;;              (setq rust-indent-offset 2)
;;              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (add-hook 'company-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;; (setq company-tooltip-align-annotations t)
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;; 				  global-semanticdb-minor-mode
;; 				  global-semantic-idle-summary-mode
;; 				  global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)

;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;;
;;; OCAML
;;;

(add-hook 'tuareg-mode-hook
          (lambda ()
            ;; Add opam emacs directory to the load-path
            (setq opam-share
                  (substring
                   (shell-command-to-string "opam config var share 2> /dev/null")
                   0 -1))
            (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
            ;; Load merlin-mode
            (require 'merlin)
            ;; Start merlin on ocaml files
            (add-hook 'tuareg-mode-hook 'merlin-mode t)
            (add-hook 'caml-mode-hook 'merlin-mode t)
            ;; Enable auto-complete
            (setq merlin-use-auto-complete-mode 'easy)
            ;; Use opam switch to lookup ocamlmerlin binary
            (setq merlin-command 'opam)
            (company-mode)
            (require 'ocp-indent)
            (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
            (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
            (autoload 'merlin-mode "merlin" "Merlin mode" t)
            (utop-minor-mode)
            (company-quickhelp-mode)
            ;; Important to note that setq-local is a macro and it needs to be
            ;; separate calls, not like setq
            (setq-local merlin-completion-with-doc t)
            (setq-local indent-tabs-mode nil)
            (setq-local show-trailing-whitespace t)
            (setq-local indent-line-function 'ocp-indent-line)
            (setq-local indent-region-function 'ocp-indent-region)
            (if (equal system-type 'darwin)
                (load-file "/Users/ben.anderson/.opam/working/share/emacs/site-lisp/ocp-indent.el")
              (load-file "/home/banderson/.opam/working/share/emacs/site-lisp/ocp-indent.el"))
            (merlin-mode)))

(add-hook 'utop-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-process "utop") nil)))

;;;
;;; Indentation Style
;;;

;;(setq c-default-style '((awk-mode . "awk")
;;						(other . "ellemtel")))

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
(add-hook 'js2-mode
          '(lambda ()
             ))
(add-hook 'js2-mode #'yas-minor-mode)

;;;
;;; JSON
;;;
(add-hook 'json-mode
          '(lambda ()
             ))

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
