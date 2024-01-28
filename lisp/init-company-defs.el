;; -*- lexical-binding: t; -*-

;; helper functions for defining custom tooltip completion
;; keystrokes (M+n in stock company-mode setting)

;; `numbered' counts from 1
(defun --number-to-keystroke (numbered keystrokes)
    (nth (mod (- numbered 1)
	      company-tooltip-limit)
	 keystrokes))

;; return value counts from 1
(defun --keystroke-to-number (keystroke keystrokes)
  (+ (cl-position keystroke keystrokes) 1))

(defun --company-show-keystrokes (format numbered)
  (format format
	  (--number-to-keystroke numbered custom-keystrokes)))

(defun company-homerow-complete-number (keystroke)
  (interactive)
  (company-complete-number
      (--keystroke-to-number keystroke custom-keystrokes)))

(defun init-company-completion-keystrokes (keystrokes keybinding-fmt)
  (interactive)
  (setq custom-keystrokes keystrokes)
  (setq company-tooltip-limit (length custom-keystrokes))
  (customize-set-variable
   'company-show-numbers-function
   (-partial '--company-show-keystrokes
	     ;; a whitespace to prevent left margin being
	     ;; chomped off
	     (concat " " keybinding-fmt))
   )
  (dotimes (ii company-tooltip-limit)
    (let ((keystroke (nth ii custom-keystrokes)))
      (define-key company-active-map
		  (kbd (format keybinding-fmt keystroke))
		  (lambda ()
		    (interactive)
		    (company-homerow-complete-number keystroke)))))
  )
;; end of helper functions for custom keystrokes

(provide 'init-company-defs)
