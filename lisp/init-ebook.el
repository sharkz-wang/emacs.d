(require-package 'pdf-tools)

(custom-set-variables
 '(pdf-tools-handle-upgrades nil)
 '(pdf-view-midnight-colors '("#F6F7EF" . "#0d0d0d" ))
 )
(setq pdf-info-epdfinfo-program "/opt/local/bin/epdfinfo")

(pdf-tools-install)

(defun init-pdf-tools-handler ()
  (interactive)

  (evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "n") 'pdf-view-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "p") 'pdf-view-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "l") 'pdf-view-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "h") 'pdf-view-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "d") 'pdf-view-scroll-up-or-next-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-scroll-down-or-previous-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "H") 'image-bob)
  (evil-define-key 'normal pdf-view-mode-map (kbd "L") 'image-eob)
  (evil-define-key 'normal pdf-view-mode-map (kbd "g g") 'pdf-view-first-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "f") 'pdf-links-action-perform)
  (evil-define-key 'normal pdf-view-mode-map (kbd "C-o") 'pdf-history-backward)
  (evil-define-key 'normal pdf-view-mode-map (kbd "o") 'pdf-outline)
  (evil-define-key 'normal pdf-view-mode-map (kbd "i") 'pdf-view-goto-page)
  (evil-define-key 'normal pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)
  (evil-define-key 'normal pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)
  (evil-define-key 'normal pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
  (evil-define-key 'normal pdf-view-mode-map (kbd "0") 'pdf-view-scale-reset)

  ;; TODO: integrate it to evil-mark
  (evil-define-key 'normal pdf-view-mode-map (kbd "m") 'pdf-view-position-to-register)
  (evil-define-key 'normal pdf-view-mode-map (kbd "`") 'pdf-view-jump-to-register)
  (evil-define-key 'normal pdf-view-mode-map (kbd "'") 'pdf-view-jump-to-register)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "SPC s s") 'pdf-occur
    (kbd "SPC i d") 'pdf-view-midnight-minor-mode
    ))

(add-hook 'pdf-view-mode-hook 'init-pdf-tools-handler)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

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
