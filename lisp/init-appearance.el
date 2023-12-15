(setq-default line-spacing 3)
(setq show-trailing-whitespace t)
(setq text-scale-mode-step 1.05)

;; setup window divider character for splitted window
;; default character ?| gives you an ugly dashed line in terminal mode
(defun --set-window-divider-character ()
  (let ((display-table (or (window-display-table)
			   buffer-display-table
			   standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook
	  '--set-window-divider-character)
(set-face-foreground 'vertical-border "#808080")

;; tty mode does not support window dividers for vertical windows.
;; using a slightly brighter mode-line to make
;; multi-window view less chaotic.
(set-face-attribute 'mode-line nil
		    :background "#555555" :foreground "#F8F8F0"
		    :weight 'light)
(set-face-attribute 'mode-line-inactive nil
		    :background "#383838" :foreground "#F8F8F0"
		    :weight 'light)

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

;; disable all loaded themes before load a new theme
;; (advice-add 'load-theme :around #'load-theme-advice)

(defun override-default-faces (f theme-id &optional no-confirm no-enable &rest args)
    "My preferred default face settings that overrides any custom themes."
    ;; use smaller mode line
    (set-face-attribute 'mode-line nil :height 210)
)
;; let's setup a init handler where we override any incoming
;; themes with our preferred defaults
(advice-add 'load-theme :after #'override-default-faces)

;; override default monokai's brownish color tone in gui emacs
(custom-set-variables
    '(monokai-highlight  "#3D3C3D")
    '(monokai-highlight-line "#3D3C3D")
    '(monokai-comments "#747474")
    '(monokai-background "#151515")
)
;; override default org-mode color tone
(custom-set-faces
    '(org-block            ((t (:background "#2D2D2D"))))
    '(org-block-begin-line ((t (:background "#373737" :height 0.7))))
    '(org-block-end-line   ((t (:background "#373737" :height 0.7))))
    '(org-table            ((t (:background "#2D2D2D"))))
    '(org-link             ((t (:foreground "#8070FF"))))
)
(require-package 'monokai-theme)

(require-package 'poet-theme)

(require 'hide-mode-line)
(require 'org-pretty-table)

(setq is-paper-imitation-theme nil)

(defun turn-on-paper-imitation-theme ()
  (interactive)
  (disable-all-themes)

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
  (load-theme 'gray-on-black t)
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

(load-theme 'monokai t)

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

;; as narrow as mode-line's font size
(setq doom-modeline-height 1)
;; e.g. from upstream's comments:
;;     'relative-from-project => emacs/lisp/comint.el
;;     'relative-to-project => lisp/comint.el
(customize-set-variable 'doom-modeline-buffer-file-name-style
			'truncate-with-project)

(if (display-graphic-p)
    (progn
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-major-mode-color-icon t))
  (progn
    (setq doom-modeline-icon nil)
    (setq doom-modeline-major-mode-icon nil)))

(setq doom-modeline-minor-modes nil)

(setq doom-modeline-persp-name t)
(setq doom-modeline-github nil)
(setq doom-modeline-version t)

(doom-modeline-mode 1)
(display-time-mode 1)

(require-package 'form-feed)

(require-package 'centered-window)
(centered-window-mode)

;; (require-package 'nyan-mode)
;; (nyan-mode t)

(global-whitespace-mode t)

(eval-after-load 'whitespace
    '(progn
	(setq whitespace-style
	      '(face                             ;; enable face
		tab-mark tabs                    ;; indicate all tabs with marks `>>'
						 ;; whitespace is visually annoying,
						 ;; so we just enable tabs
		space-after-tab space-before-tab ;; indicate mixed use of tab/whitespaces
						 ;; it won't be shown as marks
		))

	;; face for tab marks `>>'
	(set-face-attribute 'whitespace-tab nil
			    :background nil :foreground "gray30" :weight 'light)

	;; faces for warning of mixed tab/whitespace
	;; I didn't know why these two attributes was not consistent in
	;; using foreground/background, so I just set all of them
	(set-face-attribute 'whitespace-space-after-tab nil
			    :background "gray25" :foreground "gray25" :weight 'light)
	(set-face-attribute 'whitespace-space-before-tab nil
			    :background "gray25" :foreground "gray25" :weight 'light)
    )
)

(provide 'init-appearance)
