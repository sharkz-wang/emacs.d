;;; config.el --- rogue Layer config File for Spacemacs
;; Mostly theme configuration

(deftheme etbembo
  "etbembo theme")

(let*
    ((bg-white           "#fbf8ef")
     (bg-light           "#222425")
     (bg-dark            "#1c1e1f")
     (bg-darker          "#1c1c1c")
     (fg-white           "#ffffff")
     (shade-white        "#efeae9")
     (fg-light           "#655370")
     (dark-cyan          "#008b8b")
     (region-dark        "#2d2e2e")
     (region             "#39393d")
     (slate              "#8FA1B3")
     (keyword            "#f92672")
     (comment            "#525254")
     (builtin            "#fd971f")
     (purple             "#9c91e4")
     (doc                "#727280")
     (type               "#66d9ef")
     (string             "#b6e63e")
     (gray-dark          "#999")
     (gray               "#bbb")
     (sans-font          "Monaco")
     (serif-font         "Merriweather")
     (et-font            "EtBembo")
     (sans-mono-font     "Monaco")
     (serif-mono-font    "Monaco"))

  (custom-theme-set-faces
   'etbembo
   `(variable-pitch
     ((t (:family ,et-font
		  :background nil
		  :foreground ,bg-dark
		  :height 1.7))))
   `(header-line
     ((t (:background nil :inherit nil))))
   `(eval-sexp-fu-flash
     nil)
   `(eval-sexp-fu-flash-error
     nil)
   `(hackernews-link-face
     nil)
   `(hackernews-comment-count-face
     nil)
   `(company-tooltip
     nil)
   `(company-scrollbar-fg
     nil)
   `(company-scrollbar-bg
     nil)
   `(company-tooltip-common
     nil)
   `(company-tootip-annotation
     nil)
   `(company-tooltip-selection
     nil)
   `(show-paren-match
     nil)
   `(magit-section-heading
     nil)
   `(magit-header-line
     ((t (:background nil
		      :foreground ,bg-white
		      :box nil))))
   `(magit-diff-hunk-heading
     nil)
   `(magit-diff-hunk-heading-highlight
     nil)
   `(tooltip
     nil)
   `(git-gutter-fr:modified
     nil)
   `(doom-neotree-dir-face
     ((t (:family ,sans-font
		  :height 1.0))))
   `(doom-neotree-file-face
     ((t (:family ,sans-font
		  :height 1.0))))
   `(doom-neotree-text-file-face
     ((t (:family ,sans-font
		  :height 1.0))))
   `(doom-neotree-hidden-file-face
     ((t (:family ,sans-font
		  :height 1.0
		  :foreground ,comment))))
   `(doom-neotree-media-file-face
     ((t (:family ,sans-font
		  :height 1.0
		  :foreground ,type))))
   `(doom-neotree-data-file-face
     ((t (:family ,sans-font
		  :height 1.0
		  :foreground ,doc))))
   `(neo-root-dir-face
     nil)
   `(mode-line
     ((t (:background ,bg-white
		      :box nil))))
   `(mode-line-inactive
     ((t (:box nil))))
   `(powerline-active1
     ((t (:background ,bg-white))))
   `(powerline-active2
     ((t (:background ,bg-white))))
   `(powerline-inactive1
     ((t (:background ,bg-white))))
   `(powerline-inactive2
     ((t (:background ,bg-white))))
   `(highlight
     ((t (:background ,shade-white))))
   `(hl-line
     nil)
   `(solaire-hl-line-face
     nil)
   `(org-document-title
     ((t (:inherit nil
		   :family ,et-font
		   :height 2.0
		   :foreground ,bg-dark
		   :underline nil))))
   `(org-document-info
     ((t (:height 1.2
		  :slant italic))))
   `(org-meta-line
     ((t (:height 0.8
		  :foreground ,gray
		  :slant italic
		  ))))
   `(org-level-1
     ((t (:inherit nil
		   :family ,et-font
		   :height 2.0
		   :weight bold
		   :slant normal
		   :foreground ,bg-dark))))
   `(org-level-2
     ((t (:inherit nil
		   :family ,et-font
		   :weight bold
		   :height 1.5
		   :slant normal
		   :foreground ,bg-dark))))
   `(org-level-3
     ((t (:inherit nil
		   :family ,et-font
		   :weight semi-bold
		   :slant italic
		   :height 1.2
		   :foreground ,bg-dark))))
   `(org-level-4
     ((t (:inherit nil
		   :family ,et-font
		   :weight semi-bold
		   :slant italic
		   :height 1.1
		   :foreground ,bg-dark))))
   `(org-level-5
     nil)
   `(org-level-6
     nil)
   `(org-level-7
     nil)
   `(org-level-8
     nil)
   `(org-headline-done
     ((t (:family ,et-font
		  :strike-through t))))
   `(org-quote
     nil)
   `(org-block
     ((t (:background ,shade-white
		      :family ,sans-mono-font
		      :height 0.7
		      :foreground ,bg-dark))))
   `(org-block-begin-line
     ((t (:background nil
		      :height 0.4
		      :family ,sans-mono-font
		      :foreground ,gray))))
   `(org-block-end-line
     ((t (:background nil
		      :height 0.4
		      :family ,sans-mono-font
		      :foreground ,gray))))
   `(org-document-info-keyword
     ((t (:height 0.8
		   :slant italic
		  :foreground ,gray))))
   `(org-link
     ((t (:foreground ,bg-dark))))
   `(org-special-keyword
     ((t (:family ,sans-mono-font
		  :height 0.8))))
   `(org-todo
     ((t (:family ,et-font
		  :background nil
		  :foreground ,slate
		  :slant italic
		  :height 1.2))))
   `(org-done
     nil)
   `(org-agenda-current-time
     nil)
   `(org-hide
     ((t (:foreground ,bg-white))))
   `(org-indent
     ((t (:inherit (org-hide fixed-pitch)))))
   `(org-time-grid
     nil)
   `(org-warning
     nil)
   `(org-date
     ((t (:family ,sans-mono-font
		  :height 0.8))))
   `(org-agenda-structure
     nil)
   `(org-agenda-date
     ((t (:inherit variable-pitch
		   :height 1.1))))
   `(org-agenda-date-today
     nil)
   `(org-agenda-date-weekend
     nil)
   `(org-scheduled
     nil)
   `(org-upcoming-deadline
     nil)
   `(org-scheduled-today
     nil)
   `(org-scheduled-previously
     nil)
   `(org-agenda-done
     ((t (:strike-through t
			  :foreground ,doc))))
   `(org-ellipsis
     ((t (:underline nil
		     :foreground ,comment))))
   `(org-tag
     ((t (:foreground ,doc))))
   `(org-table
     ((t (:family ,serif-mono-font
		  :height 0.9
		  :background ,bg-white))))
   `(org-code
     ((t (:inherit nil
		   :family ,serif-mono-font
		   :foreground ,comment
		   :height 0.9))))
   `(font-latex-sectioning-0-face
     nil)
   `(font-latex-sectioning-1-face
     nil)
   `(font-latex-sectioning-2-face
     nil)
   `(font-latex-sectioning-3-face
     nil)
   `(font-latex-sectioning-4-face
     nil)
   `(font-latex-sectioning-5-face
     nil)
   `(font-latex-verbatim-face
     nil)
   `(spacemacs-normal-face
     nil)
   `(spacemacs-evilified-face
     nil)
   `(spacemacs-lisp-face
     nil)
   `(spacemacs-emacs-face
     nil)
   `(spacemacs-motion-face
     nil)
   `(spacemacs-visual-face
     nil)
   `(spacemacs-hybrid-face
     nil)
   `(bm-persistent-face
     nil)
   `(helm-selection
     nil)
   `(helm-match
     nil)
   `(cfw:face-title
     nil)
   `(cfw:face-holiday
     nil)
   `(cfw:face-saturday
     nil)
   `(cfw:face-sunday
     nil)
   `(cfw:face-periods
     nil)
   `(cfw:face-annotation
     nil)
   `(cfw:face-select
     nil)
   `(cfw:face-toolbar-button-off
     nil)
   `(cfw:face-toolbar-button-on
     nil)
   `(cfw:face-day-title
     nil)
   `(cfw:face-default-content
     nil)
   `(cfw:face-disable
     nil)
   `(cfw:face-today
     nil)
   `(cfw:face-toolbar
     nil)
   `(cfw:face-today-title
     nil)
   `(cfw:face-grid
     nil)
   `(cfw:face-header
     nil)
   `(cfw:face-default-day
     nil)
   `(dired-subtree-depth-1-face
     nil)
   `(dired-subtree-depth-2-face
     nil)
   `(dired-subtree-depth-3-face
     nil)
   `(dired-subtree-depth-4-face
     nil)
   `(dired-subtree-depth-5-face
     nil)
   `(dired-subtree-depth-6-face
     nil)
   `(nlinum-current-line
     ((t (:foreground ,bg-dark))))
   `(vertical-border
     nil)
   `(which-key-command-description-face
     nil)
   `(flycheck-error
     nil)
   `(flycheck-warning
     nil)
   `(font-lock-string-face
     nil)
   `(font-lock-comment-face
     ((t (:background nil
		      :foreground ,doc
		      :slant italic))))
   `(helm-ff-symlink
     nil)
   `(region
     nil)
   `(header-line
     ((t (:background nil
		      :inherit nil))))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'etbembo)
