3;;; d-mode.el --- Mode for editind D language sources

;; Author:     Jakub Kopański
;; Maintainer: Unmaintained
;; Created:    January 2007
;; Version:    0.1
;; Keywords:   d languages oop

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

;; This d language mode is based on derived-mode-ex.el by Martin Stjernholm
;; a simple example of a CC Mode derived for a new languge

;;; Code:

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'd-mode 'java-mode))

(c-lang-defconst c-identifier-ops
  d '((left-assoc ".")))

;; (c-lang-defconst c-after-id-concat-ops
;;   d '( ))

(c-lang-defconst c-cpp-include-directive
  d '("import"))

(c-lang-defconst c-assignment-operators
  d '("=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" ">>>=" "~="))

(c-lang-defconst c-overloadable-operators
  d '("+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" ">>>" "&&" "||"
      "~" "==" "!=" "<" "<=" ">" ">=" " !<>=" "<>" "<>=" "?"
      "!<=" "!<" "!>=" "!>" "!<>" "+=" "-=" "*="
      "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" ">>>=" "~="
      "[]" "()" "new" "delete"))

(c-lang-defconst c-line-comment-starter
  d "//")

;;(c-lang-defconst c-block-comment-starter
;;  d '("/*" "/+"))

(c-lang-defconst c-blocm-comment-ender
  d '("*/" "+/"))

(c-lang-defconst c-doc-comment-start-regexp
  d '("/\\*\\*" "/++" "///"))

;; (c-lang-defconst comment-start
;;   d "/+++++")
;;
;; (c-lang-defconst commenst-end
;;   d "+++++/")

;;; Keywords lists

(c-lang-defconst c-primitive-type-kwds
  d '("void" "bit" "byte" "ubyte" "short" "ushort" "int" "uint" "long" "ulong"
      "cent" "ucent" "float" "double" "real" "ifloat" "idouble" "ireal" "cfloat"
      "cdouble" "creal" "char" "wchar" "dchar"))

;; zwalone
;; (c-lang-defconst c-type-prefix-key
;;   d '("struct" "union" "enum" "class" "interface"))

(c-lang-defconst c-class-decl-kwds
  d '("class" "interface" "struct" "union"))

(c-lang-defconst c-other-block-decl-kwds
  d '("in" "out" "body" "pragma" "debug" "version"))

(c-lang-defconst c-typedef-decl-kwds
  d '("typedef" "alias"))
;; d '(append (c-lang-const c-typedef-kwds)
;; 	   '("typedef" "alias")))

(c-lang-defconst c-modifier-kwds
  d '("deprecated" "private" "package" "protected" "public" "export" "static"
      "final" "override" "abstract" "const" "auto" "align"))

;; cc-langs 1629
;; (c-lang-defconst c-decl-hangon-kwds
;;   d '(""))

(c-lang-defconst c-protection-kwds
  d '("private" "protected" "public"))


;; napewno? class nie można bo \"class Foo { ... ) foo;\" jest w D niedopuszczalne
(c-lang-defconst c-block-decls-with-vars
  d '("typedef" "alias"))

;; cc-langs 1743
;; (c-lang-defconst c-postfix-decl-kwds
;;   d '(""))

;; cc-langs 1760
;; (c-lang-defconst c-type-list-kwds
;;   d '(""))

(c-lang-defconst c-block-stmt-1-kwds
  d '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-2-kwds
  d '("for" "if" "switch" "while" "catch" "foreach" "synchronized" "with"
      "volatile" "version" "pragma"))

(c-lang-defconst c-simple-stmt-kwds
  d '("break" "continue" "goto" "return" "throw"))

;; zwalone
;; (c-lang-defconst c-paren-stmt-key
;;   d '("for" "foreach"))

(c-lang-defconst c-asm-stmt-kwds
  d '("asm"))

(c-lang-defconst c-constant-kwds
  d '("null" "false" "true"))

(c-lang-defconst c-primary-expr-kwds
  d '("this" "super"))

;; cc-langs 2073
;; (c-lang-defconst c-bitfield-kwds
;;   d '("D ma na to odzielny typ"))


(c-lang-defconst c-cpp-matchers
  d (c-lang-const c-cpp-matchers))

;; moje trefne...
(defcustom d-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in D mode.
Each list item should be a regexp matching a single identifier.")

(defconst d-font-lock-keywords-1 (c-lang-const c-matchers-1 d)
  "Minimal highlighting for D mode.")

(defconst d-font-lock-keywords-2 (c-lang-const c-matchers-2 d)
  "Fast normal highlighting for D mode.")

(defconst d-font-lock-keywords-3 (c-lang-const c-matchers-3 d)
  "Accurate normal highlighting for D mode.")

(defvar d-font-lock-keywords d-font-lock-keywords-3
  "Default expressions to highlight in D mode.")

;; (setq d-font-lock-keywords-1
;;       (list
;;        '("\\<\\(class\\)\\>[ \t]*\\(\\sw+\\)?"
;; 	 (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
;;        ))

;; (setq d-font-lock-keywords-2
;;       (append d-font-lock-keywords-1
;; 	      (list
;; 	       '(concat("\\<\\(" ,(c-lang-const c-primitive-type-kwds d) "\\)\\>" "\\([ \t*&]+\\sw+\\>\\)*")
;; 		       (1 font-lock-type-face) (2 font-lock-variable-name-face)))))

;; (setq d-font-lock-keywords-3
;;       (append d-font-lock-keywords-2
;; 	      (list
;; 	       '((c-lang-const c-assignment-operators d) . font-lock-operator-face)
;; 	       '((c-lang-const c-overloadable-operators d) . font-lock-operator-face)
;; 	       )))

(defvar d-mode-syntax-table nil
  "Syntax table used in d-mode buffers.")
(or d-mode-syntax-table
    (setq d-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table d))))

(defvar d-mode-abbrev-table nil
  "Abbreviation table used in d-mode buffers.")
(c-define-abbrev-table 'd-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar d-mode-map (let ((map (c-make-inherited-keymap)))
		     ;; Add bindings which are only useful for D
		     map)
  "Keymap used in d-mode buffers.")

(easy-menu-define d-menu d-mode-map "D Mode Commands"
		  ;; Can use `d' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "d" (c-lang-const c-mode-menu d)))

(defun d-mode ()
  "Major mode for editing D code."
 
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table d-mode-syntax-table)
  (setq major-mode 'd-mode
	mode-name "D"
	local-abbrev-table d-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  (c-init-language-vars d-mode)
  (c-common-init 'd-mode)
  (easy-menu-add d-menu)
  (run-hooks 'd-mode-common-hook)
  (run-hooks 'd-mode-hook)
  (c-update-modeline))

(provide 'd-mode)

;;; d-mode.el ends here