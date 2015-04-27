;;; yy-mode.el --- YeahYeah mode.

;; Author:     2015 Ben Anderson
;; Maintainer: Unmaintained
;; Created:    April 2015
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This an extention of cc-mode to do what I want it to do
;; becase regular ass cc-mode doesn't allow specific customizations.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'yy-mode 'c-mode))

;; Add the 'global' 'local' and 'internal' keywords to the list of
;; available modifiers
;; THIS IS THE ONLY REASON THIS MODE EXISTS
(c-lang-defconst c-modifier-kwds
  yy (append '("global" "local" "internal") (c-lang-const c-modifier-kwds)))

(defcustom yy-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in YY mode.
Each list item should be a regexp matching a single identifier.")

(defconst yy-font-lock-keywords-1 (c-lang-const c-matchers-1 yy)
  "Minimal highlighting for YY mode.")

(defconst yy-font-lock-keywords-2 (c-lang-const c-matchers-2 yy)
  "Fast normal highlighting for YY mode.")

(defconst yy-font-lock-keywords-3 (c-lang-const c-matchers-3 yy)
  "Accurate normal highlighting for YY mode.")

(defvar yy-font-lock-keywords yy-font-lock-keywords-3
  "Default expressions to highlight in YY mode.")

(defvar yy-mode-syntax-table nil
  "Syntax table used in yy-mode buffers.")
(or yy-mode-syntax-table
    (setq yy-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table yy))))

(defvar yy-mode-abbrev-table nil
  "Abbreviation table used in yy-mode buffers.")

(defvar yy-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for YY
		      map)
  "Keymap used in yy-mode buffers.")

(easy-menu-define yy-menu yy-mode-map "YY Mode Commands"
		  ;; Can use `yy' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "YY" (c-lang-const c-mode-menu yy)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[ch]\\'" . yy-mode))

;;;###autoload
(defun yy-mode ()
  "Major mode for editing YY (pronounced \"Yeah Yeah\") code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `yy-mode-hook'.

Key bindings:
\\{yy-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table yy-mode-syntax-table)
  (setq major-mode 'yy-mode
	mode-name "YY"
	local-abbrev-table yy-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars yy-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'yy-mode)
  (easy-menu-add yy-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'yy-mode-hook)
  (c-update-modeline))


(provide 'yy-mode)

;;; yy-mode.el ends here
