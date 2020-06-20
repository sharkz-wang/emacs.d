(setq-default line-spacing 3)
(setq show-trailing-whitespace t)

(add-to-list 'custom-theme-load-path
	     (expand-file-name "theme" user-emacs-directory))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  (unless no-enable
    (disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)))

(advice-add 'load-theme
            :around
            #'load-theme-advice)

(require-package 'monokai-theme)

(require-package 'poet-theme)

(require-package 'org-bullets)
(require 'hide-mode-line)
(require 'org-pretty-table)

(setq is-paper-imitation-theme nil)

(defun turn-on-paper-imitation-theme ()
  (interactive)
  (disable-all-themes)

  (setq org-startup-indented t
	org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
	org-ellipsis "  " ;; folding symbol
	;; org-pretty-entities t
	header-line-format " "
	org-hide-emphasis-markers t
	;; show actually italicized text instead of /italicized text/
	org-agenda-block-separator ""
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t)

  (org-bullets-mode 1)
  (global-org-pretty-table-mode 1)
  (variable-pitch-mode 1)
  (global-hl-line-mode -1)
  (hide-mode-line-mode 1)
  (setq line-spacing 0.1)

  (load-theme 'etbembo t)
  (load-theme 'spacemacs-light t)
  (enable-theme 'etbembo)

  (text-scale-set -7)

  (setq is-paper-imitation-theme t)
  )

(defun turn-off-paper-imitation-theme ()
  (interactive)

  (disable-all-themes)
  (load-theme 'monokai t)
  (set-frame-font "Monaco-17")

  (toggle-truncate-lines 1)
  (variable-pitch-mode -1)
  (global-hl-line-mode 1)
  (hide-mode-line-mode -1)

  (text-scale-set 0)

  (setq is-paper-imitation-theme nil)
  )

(defun toggle-paper-imitation-theme ()
  (interactive)
  (if is-paper-imitation-theme
    (turn-off-paper-imitation-theme)
    (turn-on-paper-imitation-theme)))

(turn-on-paper-imitation-theme)

(defun increse-font ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun decrese-font ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(define-key evil-normal-state-map (kbd "SPC i TAB") 'toggle-paper-imitation-theme)
(define-key evil-normal-state-map (kbd "SPC i +") 'increse-font)
(define-key evil-normal-state-map (kbd "SPC i =") 'increse-font)
(define-key evil-normal-state-map (kbd "SPC i -") 'decrese-font)

(setq text-scale-mode-step 1.05)

(require-package 'moe-theme)
(require 'moe-theme)

(require-package 'solarized-theme)
(require-package 'dracula-theme)
(require-package 'ujelly-theme)

(require-package 'zone-rainbow)

(require-package 'dashboard)
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '(agenda
			(recents  . 10)
			(projects . 10)
			(bookmarks . 10)
			))
(setq show-week-agenda-p t)

;; (dashboard-setup-startup-hook)
;; 1. needs a hook that invoked right after dashboard buffer inited
;; 2. dashboard-next-section not working through elisp
;; (add-hook 'after-init-hook (lambda ()
				 ;; (with-current-buffer "*dashboard*"
				 ;; (next-line 10))
				 ;; ))

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

;; use absolute line number as current symbol
(customize-set-variable 'linum-relative-current-symbol "")

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
(display-time-mode 1)

(require-package 'form-feed)

(provide 'init-appearance)
