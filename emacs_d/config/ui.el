;;; +===========================================================+
;;; | Global Interface Configuration                            |
;;; +===========================================================+

; Set the font to something sane
(if (>= emacs-major-version 23)
	(if (string= "darwin" system-type)
		(modify-all-frames-parameters
		 '((font . "Menlo-12")))
	  (modify-all-frames-parameters
	   '((font . "Monospace-12")))
))


; force utf-8
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mac-allow-anti-aliasing t)

; GNU splash screen
(setq inhibit-startup-message t)
(global-font-lock-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;(set-scroll-bar-mode 'right)
(setq truncate-partial-width-windows t)
(display-time)

(when (boundp 'emacsd-tile)
  (require 'emacsd-tile))

; Highlight current line
(global-hl-line-mode 1)

(show-paren-mode t)
(transient-mark-mode t)
;;(set-background-color "#1A1A1A")
;;(set-foreground-color "#F5F5F5")

(defun color-theme-semantic-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-semantic-monokai
     ;; Frame Parameters
     ((background-color . "#272822")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#F8F8F0"))
     ;; Main
     ;; (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-builtin-face ((t (:weight bold))))
     (font-lock-comment-face ((t (:foreground "#75715E" :slant italic))))
     ;;(font-lock-comment-delimiter-face ((t (:slant italic))))
     ;;(font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-constant-face ((t (:foreground nil))))
     (font-lock-doc-face ((t (:slant italic))))
     (font-lock-doc-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-function-name-face ((t (:foreground "#A6E22E"))))
     ;; (font-lock-keyword-face ((t (:foreground "#F92672"))))
     (font-lock-keyword-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:weight bold))))
     ;;(font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-string-face ((t (:foreground "#F92672"))))
     (font-lock-type-face ((t (:foreground "#89BDFF"))))
     ;; (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     ;; Main - #2
     (hl-line ((t (:background "#141411"))))
     (minibuffer-prompt ((t (:foreground "#75715E"))))
     (modeline ((t (:background "#595959" :foreground "#E6E6E6"))))
     (region ((t (:background "#383830"))))
     (show-paren-match-face ((t (:background "#383830"))))
     ;; CUA
     (cua-rectangle ((t (:background "#141411"))))
     ;; IDO
     (ido-first-match ((t (:foreground "#AE81FF"))))
     (ido-only-match ((t (:foreground "#A6E22A"))))
     (ido-subdir ((t (:foreground "#89BDFF"))))
     ;; Misc
     (yas/field-highlight-face ((t (:background "#383830")))))))
