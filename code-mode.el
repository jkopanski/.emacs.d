(require 'cc-mode)
(require 'yasnippet-bundle)
;; (require 'linum)
;; (require 'd-mode)

; dodatkowe face'e
(make-face 'font-lock-operator-face)
(make-face 'font-lock-preprocessor-face)
(make-face 'font-lock-numbers-face)
(setq font-lock-operator-face 'font-lock-operator-face)

; "this is my coding style" ;)
(c-add-style "ksh-style"
	     ;'("ellemtel"
	     '("python"
	       (indent-tabs-mode . nil)  ; używaj spacji zamiast tabów
	       (c-basic-offset . 2)      ; wcięcia o długości 2 spacji
	       (c-toggle-auto-state 1)   ; automatyczne wcięcia
	       (c-toggle-hungry-state 1) ; usuwaj białe znaki aż do następnego tekstu
	       (c-offsets-alist . (
				   (innamespace . 0)
				   ))
	       (setq c-hanging-braces-alist
		     '(
		       (brace-list-open)
		       (brace-entry-open)
		       (statement-cont)
		       (substatement-open after)
		       (block-close . c-snug-do-while)
		       (extern-lang-open after)
		       (namespace-open after)
		       (module-open after)
		       (composition-open after)
		       (inexpr-class-open after)
		       (inexpr-class-close before)
		       )
		     )))

(setq std-types
      '("short" "int" "long" "float" "double"
	"char" "signed" "unsigned" "bool"
	"enum" "void" "string"))

(setq extra-types
      '("uint" "ulong" "ushort" "uchar" "uint8" "int8" "uint16" "int16" "uint32" "int32" "uint64" "int64"
	"int8_t" "int16_t" "int32_t" "int64_t" "uint8_t" "uint16_t" "uint32_t" "uint64_t" 
	"s8" "s16" "s32" "s64" "u8" "u16" "u32" "u64" "float32" "float64" "f32" "f64" "f128" "byte" "BYTE" "word" "WORD" "dword" "DWORD"))

(setq opengl-types
      '("GLboolean" "GLbyte" "GLubyte" "GLchar" "GLshort" "GLushort" "GLint" "GLuint" "GLsizei" "GLenum"
	"GLintptr" "GLsizeiptr" "GLbitfield" "GLfloat" "GLclampf" "GLdouble" "GLclampd"))

(setq openal-types
      '("ALboolean" "ALbyte" "ALubyte" "ALshort" "ALushort" "ALint" "ALuint" "ALsizei" "ALenum"
	"ALbitfield" "ALfloat" "ALclampf" "ALdouble" "ALclampd"))

(setq xorg-types
      '("Window" "Display" "XVisualInfo" "XRRScreenConfiguration" "GLXWindow" "GLXContext" "GLXFBConfig"))

(defvar keywords
  '(("and\\|and_eq\\|\\|asm\\|auto\\|bitand\\|bitor\\|break\\|case\\|catch\\|class\\|compl\\|const\\|const_cast\\|continue\\|default\\|delete\\|do\\|dynamic_cast\\|else\\|enum\\|explicit\\|export\\|extern\\|for\\|friend\\|goto\\|if\\|inline\\|mutable\\|namespace\\|new\\|not\\|not_eq\\|or\\|or_eq\\|public\\|private\\|protected\\|register\\|reinterpret_cast\\|return\\|sizeof\\|static\\|static_cast\\|struct\\|switch\\|template\\|this\\|throw\\|try\\|typedef\\|typeid\\|typename\\|union\\|using\\|virtual\\|volatile\\|while\\|xor\\|xor_eq" . font-lock-keyword-face)))

(defvar operators
  '(("[][(){}]\\|~\\|!\\|%\\|&\\|*\\|-\\|+\\|=\\||\\|:\\|;\\|<\\|>\\|,\\|/\\|\\.\\|\\^" . font-lock-operator-face)))

;(defvar numbers
;  '(("[0-9]" . font-lock-numbers-face)))

(setq c++-font-lock-extra-types (append extra-types opengl-types openal-types xorg-types))
(setq c-font-lock-extra-types c++-font-lock-extra-types)

;(font-lock-add-keywords 'c-mode keywords)
(font-lock-add-keywords 'c-mode operators)
;(font-lock-add-keywords 'c-mode numbers)
;(font-lock-add-keywords 'c++-mode keywords)
(font-lock-add-keywords 'c++-mode operators)
(font-lock-add-keywords 'd-mode operators)
;(font-lock-add-keywords 'c++-mode numbers)

(defun ksh-c++-mode-hook ()
  (c-set-style "ksh-style")

  ; klawisze
  (define-key c++-mode-map [f1] 'manual-entry)  ; wywołaj man
  (define-key c++-mode-map [f2] 'info)          ; wywołaj info
  (define-key c++-mode-map [f3] 'next-error)
  (define-key c++-mode-map [f4] 'previous-error)
  (define-key c++-mode-map [f5] 'gdb)           ; debugger
  (define-key c++-mode-map [f7] 'compile)
  (define-key c++-mode-map [f10] 'gud-break)
  ; setq
  (setq Man-notify-method 'newframe)            ; otwórz mana w nowym oknie
;  (ede-minor-mode 1)
)

(defun ksh-d-mode-hook ()
  (c-set-style "ksh-style")
;;  (define-key d-mode-map [f1] 'manual-entry)  ; wywołaj man mamy jakies manuale dla d ?
;;  (define-key d-mode-map [f2] 'info)          ; wywołaj info j/w
  (define-key d-mode-map [f3] 'next-error)
  (define-key d-mode-map [f4] 'previous-error)
  (define-key d-mode-map [f5] 'gdb)           ; debugger
  (define-key d-mode-map [f7] 'compile)
  (define-key d-mode-map [f10] 'gud-break)
)

(defun ksh-csharp-mode-hook ()
  (c-set style "ksh-style")

;;  (define-key d-mode-map [f1] 'manual-entry)  ; wywołaj man mamy jakies manuale dla c# ?
;;  (define-key d-mode-map [f2] 'info)          ; wywołaj info j/w
  (define-key d-mode-map [f3] 'next-error)
  (define-key d-mode-map [f4] 'previous-error)
  (define-key d-mode-map [f5] 'gdb)           ; debugger
  (define-key d-mode-map [f7] 'compile)
  (define-key d-mode-map [f10] 'gud-break)
)

(defun ksh-asm86-mode-hook ()

  (setq asm86-author "Jakub \"Nat\" Kopanski")
  (setq asm86-electric-gap-size          8)
  (setq asm86-variable-base-offset       4)
  (setq asm86-tab-entry-base-offset      4)
  (setq asm86-blank-base-offset          4)
  (setq asm86-code-comment-base-offset   4)
  (setq asm86-inline-comment-base-offset 4)
  (setq asm86-inst-base-offset           4)
  (setq asm86-blank-func-offset          4)
  (setq asm86-label-func-offset          4)
  (setq asm86-header-comment-func-offset 4)
  (setq asm86-code-comment-func-offset   4)
  (setq asm86-inline-comment-func-offset 4)
  (setq asm86-inst-func-offset           4)

)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "polish") "english" "polish")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    )
  )

;(defun nat-latex-mode-hook ()
;  (flyspell-mode)
;  (ispell-change-dictionary "polish")
;  )

(add-hook 'c-mode-hook 'ksh-c++-mode-hook)
(add-hook 'c++-mode-hook 'ksh-c++-mode-hook)
(add-hook 'c-mode-hook 'ksh-d-mode-hook)
(add-hook 'd-mode-hook 'ksh-d-mode-hook)
(add-hook 'csharp-mode-hook 'ksh-csharp-mode-hook)
(add-hook 'asm86-mode-hook 'ksh-asm86-mode-hook)
(add-hook 'vhdl-mode-hook 'ksh-vhdl-mode-hook)
(add-hook 'LaTeX-mode-hook 'nat-latex-mode-hook)
