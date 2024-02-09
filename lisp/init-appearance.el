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

;; Somewhere in the emacs bootstrap sequence,
;; the following line of code choose a premature candidate of ...
;;     (or (window-display-table)
;;         buffer-display-table
;;         standard-display-table)))
;; , leaving all whitespaces displayed as dots that won't go away
;; unless you trigger an refresh by, for example, splitting windows.
;;
;; Simply adding the same setup function to `window-setup-hook' does
;; not tame this mess, you have to hook the setup function
;; only after `window-setup-hook' is fired.
(add-hook 'window-setup-hook
	  (lambda ()
	    (add-hook 'window-configuration-change-hook
		      '--set-window-divider-character)))
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

;; maximize gui screen on start-up
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

(require-package 'hide-mode-line)
(require 'org-pretty-table)

(load-theme 'monokai t)
(require-package 'dracula-theme)
(require-package 'zone-rainbow)

(require-package 'hl-anything)
(global-hl-line-mode)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-width 0)

;; setup highlight face
(custom-set-faces
 '(line-number-current-line ((t :inherit hl-spotlight
				:foreground "#FF8700"))))

(evil-global-set-key 'normal (kbd "SPC t l") 'global-display-line-numbers-mode)

(require-package 'highlight-parentheses)
(global-highlight-parentheses-mode 1)

;; disable inverse video of parentheses highlight, which makes
;; make cursor brighter than hinted paren
(set-face-attribute 'show-paren-match nil :inverse-video nil)

(require-package 'doom-modeline)

;; as narrow as mode-line's font size
(setq doom-modeline-height 1)
;; e.g. from upstream's comments:
;;     'relative-from-project => emacs/lisp/comint.el
;;     'relative-to-project => lisp/comint.el
(customize-set-variable 'doom-modeline-buffer-file-name-style
			'relative-from-project)

(if (display-graphic-p)
    (progn
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-major-mode-color-icon t))
  (progn
    (setq doom-modeline-icon nil)
    (setq doom-modeline-major-mode-icon nil)))

;; yes I like it simpler
(with-eval-after-load "doom-modeline"
  (doom-modeline-def-modeline 'main
  '(persp-name buffer-info buffer-position) '()))

(setq doom-modeline-position-column-line-format '("%l"))
(setq doom-modeline-total-line-number t)

(doom-modeline-mode 1)

(require-package 'form-feed)

(require-package 'centered-window)
(centered-window-mode)

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
			    :background 'unspecified :foreground "gray30" :weight 'light)

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
