;;; +===========================================================+
;;; | Global Interface Configuration                            |
;;; +===========================================================+

; Set the font to something sane
(if (>= emacs-major-version 23)
	(if (string= "darwin" system-type)
		(modify-all-frames-parameters
		 '((font . "Andale Mono-12")))
	  (modify-all-frames-parameters
	   '((font . "")))
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

(setq mac-allow-anti-aliasing nil)

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

;; (custom-set-faces
;;  ;'(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
;;  '(font-lock-comment-face ((t (:foreground "#95917E"))))
;;  '(font-lock-constant-face ((t (:bold t :foreground "#66D9EF" :background "dark"))))
;;  ;'(font-lock-doc-string-face ((t (:foreground "green2"))))
;;  '(font-lock-function-name-face ((t (:foreground "#A6E22E"))))
;;  '(font-lock-keyword-face ((t (:bold t :foreground "#F92672"))))
;;  ;'(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
;;  ;'(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
;;  '(font-lock-string-face ((t (:foreground "#E6DB74"))))
;;  '(font-lock-type-face ((t (:foreground "#A6E22E"))))
;;  '(font-lock-variable-name-face ((t (:foreground "#98FBFF"))))
;;  ;'(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
;;  '(highlight ((t (:background "gray20"))))
;;  ;'(list-mode-item-selected ((t (:background "gold"))))
;;  ;'(makefile-space-face ((t (:background "wheat"))))
;;  '(mode-line ((t (:background "#BFBFBF" :foreground "#000000" :box (:line-width -1 :color "#666666")))))
;;  ;'(paren-match ((t (:background "darkseagreen4"))))
;;  '(region ((t (:foreground "#272822" :background "#66D9EF"))))
;;  ;'(show-paren-match ((t (:foreground "black" :background "wheat"))))
;;  ;'(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
;;  '(speedbar-button-face ((t (:foreground "green4" :background "dark"))))
;;  '(speedbar-directory-face ((t (:foreground "khaki" :background "dark"))))
;;  '(speedbar-file-face ((t (:foreground "cyan" :background "dark"))))
;;  '(speedbar-tag-face ((t (:foreground "Springgreen" :background "dark"))))
;;  '(vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue" :background "dark"))))
;;  '(vhdl-speedbar-entity-face ((t (:foreground "darkGreen" :background "dark"))))
;;  '(vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "darkGreen" :background "dark"))))
;;  '(vhdl-speedbar-package-face ((t (:foreground "black" :background "dark"))))
;;  '(vhdl-speedbar-package-selected-face ((t (:underline t :foreground "black" :background "dark"))))
;;  '(widget-field ((t (:background "DarkBlue" :background "light")))))
