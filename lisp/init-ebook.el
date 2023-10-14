(require-package 'pdf-tools)

;; set `pdf-info-epdfinfo-program' to bin/epdfinfo relative to
;; the package dir of 'pdf-tools
;; e.g., ~/.emacs.d/elpa/pdf-tools-20220522.13/bin/epdfinfo
(setq pdf-info-epdfinfo-program
      (concat-path (package-desc-dir
		    (nth 1 (assq 'pdf-tools package-alist)))
		   "bin" "epdfinfo"))
(make-directory (file-name-directory pdf-info-epdfinfo-program) t)

;; white-on-black
;; (customize-set-variable 'pdf-view-midnight-colors (cons "#F8F8F2" "#151515"))
;; black-on-gray
(customize-set-variable 'pdf-view-midnight-colors (cons "#000000" "#D7D7D7"))

;; pdf-tools-install needs to be called to make pdf-view-mode working
(pdf-tools-install t)

(defun init-pdf-tools-handler ()
  (interactive)

  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)
   '(pdf-view-resize-factor 1.05)
   '(pdf-view-midnight-colors '("#FEFEFE" . "#0d0d0d" ))
   )

  (pdf-view-midnight-minor-mode)

  ;; make next/previous-page command start at page top
  (advice-add 'pdf-view-next-page :before
	      (lambda (&optional N) (interactive) (image-previous-line (window-vscroll))))
  (advice-add 'pdf-view-previous-page :before
	      (lambda (&optional N) (interactive) (image-previous-line (window-vscroll))))

  (evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "n") 'pdf-view-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "p") 'pdf-view-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "l") (lambda ()
							 (interactive)
							 (image-forward-hscroll 3)))
  (evil-define-key 'normal pdf-view-mode-map (kbd "h") (lambda ()
							 (interactive)
							 (image-backward-hscroll 3)))
  (evil-define-key 'normal pdf-view-mode-map (kbd "d") 'pdf-view-scroll-up-or-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-scroll-down-or-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "H") 'image-bob)
  (evil-define-key 'normal pdf-view-mode-map (kbd "L") 'image-eob)
  (evil-define-key 'normal pdf-view-mode-map (kbd "g g") (lambda ()
							   (interactive)
							   (pdf-view-first-page)
							   (image-previous-line (window-vscroll))))
  (evil-define-key 'normal pdf-view-mode-map (kbd "G") (lambda ()
							   (interactive)
							   (pdf-view-last-page)
							   (image-previous-line (window-vscroll))
							   (image-next-line (car (pdf-view-current-window-size)))
							   ))

  (evil-define-key 'normal pdf-view-mode-map (kbd "0") (lambda ()
							 (interactive)
							 (image-set-window-hscroll 0)
							 ))
  (evil-define-key 'normal pdf-view-mode-map (kbd "$") (lambda ()
							 (interactive)
							 (image-set-window-hscroll 0)
							 (image-forward-hscroll (cdr (pdf-view-image-size)))
							 ))
  (evil-define-key 'normal pdf-view-mode-map (kbd "SPC 4") (lambda ()
							 (interactive)
							 (image-set-window-hscroll 0)
							 (image-forward-hscroll (cdr (pdf-view-image-size)))
							 ))

  (evil-define-key 'normal pdf-view-mode-map (kbd "f") 'pdf-links-action-perform)
  (evil-define-key 'normal pdf-view-mode-map (kbd "C-o") 'pdf-history-backward)
  (evil-define-key 'normal pdf-view-mode-map (kbd "o") (lambda ()
							 (interactive)
							 (pdf-outline)
							 (outline-show-all)
							 (pdf-outline-move-to-current-page)))
  (evil-define-key 'normal pdf-view-mode-map (kbd "i") 'pdf-view-goto-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "=") (lambda ()
							 (interactive)
							 (pdf-view-enlarge pdf-view-resize-factor)
							 (image-forward-hscroll 1)
							 (image-backward-hscroll 1)
							 ))
  (evil-define-key 'normal pdf-view-mode-map (kbd "+") (lambda ()
							 (interactive)
							 (pdf-view-enlarge pdf-view-resize-factor)
							 (image-forward-hscroll 1)
							 (image-backward-hscroll 1)
							 ))
  (evil-define-key 'normal pdf-view-mode-map (kbd "-") (lambda ()
							 (interactive)
							 (pdf-view-shrink pdf-view-resize-factor)
							 (image-backward-hscroll 1)
							 (image-forward-hscroll 1)
							 ))

  (defun set-pdf-view-display-size-to-window-factor (factor)
    (interactive)
    (pdf-view-fit-width-to-window)
    (pdf-view-shrink factor)
    )

  (evil-define-key 'normal pdf-view-mode-map (kbd "w") (lambda ()
							 (interactive)
							 (set-pdf-view-display-size-to-window-factor 1.4)
							 ))
  (evil-define-key 'normal pdf-view-mode-map (kbd "W") 'pdf-view-fit-width-to-window)
  (evil-define-key 'normal pdf-view-mode-map (kbd "H") 'pdf-view-fit-height-to-window)

  (evil-define-key 'normal pdf-view-mode-map (kbd "a") 'pdf-annot-list-annotations)
  (evil-define-key 'normal pdf-annot-list-mode-map (kbd "TAB") 'pdf-annot-list-display-annotation-from-id)
  (evil-define-key 'normal pdf-annot-list-mode-map (kbd "RET") 'tablist-find-entry)
  (evil-define-key 'normal pdf-annot-list-mode-map (kbd "q") 'tablist-quit)

  (evil-define-key 'visual pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (evil-define-key 'normal pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  (evil-define-key 'normal pdf-view-mode-map [down-mouse-1] 'pdf-view-mouse-set-region)

  ;; TODO: integrate it to evil-mark
  (evil-define-key 'normal pdf-view-mode-map (kbd "m") 'pdf-view-position-to-register)
  (evil-define-key 'normal pdf-view-mode-map (kbd "'") 'evil-goto-global-mark-line)
  (evil-define-key 'normal pdf-view-mode-map (kbd "`") 'pdf-view-jump-to-register)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "SPC s s") 'pdf-occur
    (kbd "SPC i d") 'pdf-view-midnight-minor-mode
    )

  (evil-define-key 'normal pdf-occur-buffer-mode-map (kbd "RET") 'pdf-occur-goto-occurrence)

  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "RET") (lambda ()
								     (interactive)
								     (pdf-outline-follow-link)
								     (delete-other-windows)
								     ;; hacky trick to force redrawing window
								     (pdf-view-goto-page (pdf-view-current-page))))
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "TAB") 'outline-toggle-children)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "gk") 'outline-backward-same-level)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "gj") 'outline-forward-same-level)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "gh") 'outline-up-heading)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "gl") 'outline-next-visible-heading)

  (evil-define-key 'normal pdf-view-mode-map (kbd "SPC w /") (lambda ()
							       (interactive)
							       (pdf-view-fit-width-to-window)
							       (split-window-right)
							       (other-window 1)
							       ))
  )

(add-hook 'pdf-view-mode-hook 'init-pdf-tools-handler)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(prefer-coding-system 'utf-8)
(require-package 'nov)
(setq nov-text-width 80)
(custom-set-variables
 '(nov-variable-pitch nil))

(defun init-nov-handler ()

  (sublimity-mode 1)

  (evil-define-key 'normal nov-mode-map (kbd "o") 'nov-goto-toc)
  (evil-define-key 'normal nov-mode-map (kbd "d") (lambda () (interactive)
						    (evil-window-bottom)
						    (recenter)))
  (evil-define-key 'normal nov-mode-map (kbd "u") (lambda () (interactive)
						    (evil-window-top)
						    (recenter)))
  (evil-define-key 'normal nov-mode-map (kbd "n") 'nov-next-document)
  (evil-define-key 'normal nov-mode-map (kbd "p") 'nov-previous-document)

  (evil-leader/set-key
    "tw-" (lambda () (interactive)
	    (setq-local sublimity-attractive-centering-width
		  (- sublimity-attractive-centering-width 5))
	    (setq-local nov-text-width sublimity-attractive-centering-width)
	    ;; hacky trick to force redrawing screen
	    (switch-to-last-buffer)
	    (switch-to-last-buffer)
	    (nov-render-document)
	    )
    "tw=" (lambda () (interactive)
	    (setq-local sublimity-attractive-centering-width
		  (+ sublimity-attractive-centering-width 5))
	    (setq-local nov-text-width sublimity-attractive-centering-width)
	    ;; hacky trick to force redrawing screen
	    (switch-to-last-buffer)
	    (switch-to-last-buffer)
	    (nov-render-document)
	    )
    "tw0" (lambda () (interactive)
	    (setq-local sublimity-attractive-centering-width 80)
	    (setq-local nov-text-width sublimity-attractive-centering-width)
	    ;; hacky trick to force redrawing screen
	    (switch-to-last-buffer)
	    (switch-to-last-buffer)
	    (nov-render-document)
	    )
    )
  )
(add-hook 'nov-mode-hook 'init-nov-handler)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(provide 'init-ebook)
