;;; -*- mode: emacs-lisp; -*-

(require 'cl)
;;; +===========================================================+
;;; | Paths                                                     |
;;; +===========================================================+

(defvar emacs-root
  (if (or (eq system-type 'cygwin)
	  (eq system-type 'gnu/linux)
	  (eq system-type 'linux)
	  (eq system-type 'darwin))
      "~/.emacs.d/"
    "")
  "Root of emacs configuration files")

(require 'package)
(setq package-archives '(("gnu". "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(setq package-pinned-packages
      '((cide           . "melpa-stable")
        (clojure-mode   . "melpa-stable")
        (paredit        . "melpa-stable")
        ;;(magit          . "melpa-stable")
        (js2-mode       . "melpa-stable")
        (json-mode      . "melpa-stable")
        (terraform-mode . "melpa-stable")
        (toml-mode      . "melpa-stable")
        ;;(rust-mode      . "melpa-stable")
        (markdown-mode  . "melpa-stable")
        ))

(package-initialize)

(require 'color-theme)
(setq color-theme-is-global t)
;;(color-theme-monokai)

(cl-labels
    ((add-path (path &optional auto-load)
	       (let ((full-path (concat emacs-root path)))
		 (cond ((null auto-load) (add-to-list 'load-path full-path))
		       (t (progn
			    (add-to-list 'load-path full-path)
			    (mapc (lambda (lib) (load-library lib))
				  (directory-files full-path nil ".*\.el$"))))))))

  (add-path "plugins")    ;; modes
  (add-path "config" t))  ;; personal configurations

(color-theme-semantic-monokai)

;;; +===========================================================+
;;; | Misc mode Options                                         |
;;; +===========================================================+

(setq path "/opt/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/Users/ben.anderson/.cargo/bin")
(setenv "PATH" path)
;(setq racer-rust-src-path "/Users/ben.anderson/work/external/rust/src")
(desktop-save-mode 1)

;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (setenv "PATH"
;;                       (with-temp-buffer
;;                         (call-process "/bin/bash"
;;                                       nil (list (current-buffer) nil) nil
;;                                       "-l" "-c" "printf %s \"$PATH\"")
;;                         (buffer-string)))))

;2011-10-27 22:41:11-0500
(setq time-stamp-format "%Y-%m-%d %T%z")
(setq time-duration-format "")
(add-hook 'before-save-hook 'time-stamp)
(line-number-mode 1)
(column-number-mode 1)
(ido-mode t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)

;;; +===========================================================+
;;; | Org mode Options                                          |
;;; +===========================================================+

;;; +===========================================================+
;;; | Functions                                                 |
;;; +===========================================================+

(defun upward-find-file-pattern (pattern &optional startdir)
  "Move up directories until a file that matches the pattern is
   found. Returns the full pathname of the first file that matches."
  (let* ((dir (expand-file-name (if startdir startdir ".")))
	 (files (directory-files dir nil pattern nil))
	 (file nil))
    (while (null file)
      (cond ((not (null files)) (setq file (car files)))
	    ((string= (expand-file-name dir) "/") (setq file ""))
	    (t (progn
		 (setq dir (expand-file-name ".." dir))
		 (setq files (directory-files dir nil pattern nil))))))
    (when (not (string= file ""))
      (concat dir "/" file))))

(defun insert-date-time-stamp ()
  "Inserts the current date and time into the current buffer."
  (interactive)
  (insert (format-time-string time-stamp-format (current-time))))

;;; +===========================================================+
;;; | Configurations                                            |
;;; +===========================================================+

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(js2-indent-on-enter-key t)
;;  '(js2-strict-inconsistent-return-warning nil)
;;  '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black")))))

(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 4)
 '(json-reformat:indent-width 4)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (flycheck magit-popup lsp-ui rust-mode cargo color-identifiers-mode company-lsp yasnippet lsp-mode lsp-rust go-mode cider flycheck-rust caml json-mode toml-mode terraform-mode reykjavik-theme paredit org markdown-mode magit js2-mode fsm fringe-helper flycheck-google-cpplint emacsd-tile dash-at-point color-theme-monokai ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray50")))))
