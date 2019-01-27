(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require-package 'monokai-theme)
(load-theme 'monokai t)

(require-package 'moe-theme)
(require 'moe-theme)

(require-package 'solarized-theme)
(require-package 'dracula-theme)
(require-package 'ujelly-theme)

(require-package 'zone-rainbow)

(require-package 'dashboard)
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '(
			agenda
			(recents  . 10)
			(projects . 10)
			(bookmarks . 10)
			))
(setq show-week-agenda-p t)
(dashboard-setup-startup-hook)

(require-package 'hl-anything)
(global-hl-line-mode)

(require-package 'linum-relative)
(require 'linum-relative)

;; set linum default to relative mode by
;; setting `linum-format' to literal `linum-relative'.
;; (literal `linum-relative' is used in upstream linum-relative)
;; to set it default to absolute mode,
;; just set `linum-format' to function `linum-format-func'.
;; (original linum-mode does not support hl-anything,
;;  it's an hack and not consistent)
(setq linum-format 'linum-relative)

;; setup highlight face
(custom-set-faces
 '(linum-relative-current-face ((t :inherit hl-spotlight :foreground "#FF8700"))))

;; advice linum-mode to remember current line number
(defadvice linum-update (around hl-linum-update)
		     (let ((linum-current-line-number (line-number-at-pos)))
			       ad-do-it))
(ad-activate 'linum-update)

;; custom `linum-format' function to make vanilla linum
;; support hl-anything
(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face
			   (if (eq linum-current-line-number line)
			   'linum-relative-current-face
			   '((t :inherit linum))))))

;; returns format strings with appropriate left align width.
;; it returns different format string (just an inconsistency again -
;; in linum-relative and vanilla linum, they use different
;; printf placeholder %s and %d as numbering format) according
;; to variable `linum-format'.
(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (if (eq linum-format 'linum-format-func)
		(setq-local linum-format-fmt
			    (let ((w (length (number-to-string
					      (count-lines (point-min) (point-max))))))
			      (concat " %" (number-to-string w) "d   ")))
	      (setq linum-relative-format
		    (let ((w (length (number-to-string
				      (count-lines (point-min) (point-max))))))
		      (concat " %" (number-to-string w) "s   "))))))


(defun toggle-linum-mode ()
  (interactive)
  (if (bound-and-true-p linum-mode)
      (progn
	(linum-mode 0)
	)
    (progn
      (linum-relative-mode 1)
      (linum-mode 1)
      (setq left-fringe-width 0)
      )
    )
  ;; force new display take effect
  (switch-to-last-buffer)
  (switch-to-last-buffer)
  )

(evil-leader/set-key
  "tl" 'toggle-linum-mode
  "tr" 'toggle-relative-linum
  "tt" 'toggle-truncate-lines
  )

;; toggling relative mode
(defun toggle-relative-linum ()
  (interactive)
  (if (eq linum-format 'linum-format-func)
      (setq linum-format 'linum-relative)
    (setq linum-format 'linum-format-func)
    )
  )

(require-package 'highlight-parentheses)
(global-highlight-parentheses-mode 1)

(require-package 'all-the-icons)
(require 'all-the-icons)
;; (all-the-icons-install-fonts)

(require-package 'doom-modeline)
(require 'doom-modeline)

(setq doom-modeline-height 25)
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

(setq doom-modeline-minor-modes nil)

(setq doom-modeline-persp-name t)
(setq doom-modeline-github nil)
(setq doom-modeline-version t)

(doom-modeline-mode 1)

(require-package 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-attractive)

(setq sublimity-attractive-centering-width 80)

(evil-leader/set-key
  "tw" (lambda () (interactive)
	 (if (bound-and-true-p sublimity-mode)
	     (progn
	       (sublimity-mode -1)
	       (redraw-display)
	       )
	   (progn
	     (sublimity-mode 1)
	     ;; hacky trick to force redrawing screen
	     (switch-to-last-buffer)
	     (switch-to-last-buffer)
	     )
	   )
	 )
  )

(provide 'init-appearance)
