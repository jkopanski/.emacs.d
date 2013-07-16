(setq display-time-day-and-date nil
      display-time-24hr-format t
      display-time-mail-file t)
(display-time)

(prefer-coding-system 'utf-8)  ; UTF-8

; klawisze niezależne od trybu
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(line-number-mode 1)      ; wyświetlanie nr linii
(column-number-mode 1)    ; wyświetlanie nr kolumny
(mouse-wheel-mode 1)      ; zróbmy użytek z rolki w myszce

(global-font-lock-mode 1) ; włącza kolorki
(global-hl-line-mode 1)   ; podświetlanie bieżącej linii
;(set-face-background 'highlight "DarkSlateBlue")
;(set-face-background 'highlight "Grey30")


(delete-selection-mode 1) ; możliwość usuwania zanaczenia
;(pc-selection-mode)       ; zaznacznie tekstu z przyciśniętym shiftem

; AUCTeX stuff
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; załaduj pozostałe pliki
(setq load-path (append load-path '("~/.emacs.d/")))
(setq custom-theme-load-path (append custom-theme-load-path '("~/.emacs.d/emacs-color-theme-solarized")))
; pre 24 theme enable
;(enable-theme 'solarized-dark)
(load-theme 'solarized-dark t)
(load "modes.el")
(load "code-mode.el")
(load "modes/d-mode.el")
;(load "modes/linum.el")
(load "modes/csharp-mode.el")
(load "modes/cg-mode.el")
(load "modes/asm86-mode.el")
(load "modes/gas-mode.el")
(load "modes/spice-mode.el")
(load "modes/skill-mode.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
