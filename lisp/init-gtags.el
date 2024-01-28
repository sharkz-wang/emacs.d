(require-package 'ggtags)
(require-package 'helm-gtags)

;; Making GNU Global support more languages
;; 1) Install Exuberant Ctags
;; 2) Run `pip install pygments`
;; 3) Copy /usr/local/share/gtags/gtags.conf to ~/.globalrc
;; 4) Update ~/.globalrc: change pigments-parser.la and exuberant-ctags.la to *.so and correct their path
;; 5) Build tags by running `gtags --gtagslabel=pygments`
(require-package 'helm-gtags)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(add-hook 'helm-gtags-mode-hook
	  (lambda ()
	    (interactive)
	    (setq
	     helm-gtags-ignore-case t
	     helm-gtags-auto-update t
	     helm-gtags-use-input-at-cursor t
	     helm-gtags-pulse-at-cursor t
	     helm-gtags-prefix-key "\C-c g"
	     helm-gtags-suggested-key-mapping t
	     )

	    (define-key helm-gtags-mode-map (kbd "C-c g S") 'helm-gtags-select)
	    (define-key evil-normal-state-map (kbd "SPC g S") 'helm-gtags-select)
	    (define-key helm-gtags-mode-map (kbd "C-c g d") 'helm-gtags-dwim)
	    (define-key evil-normal-state-map (kbd "SPC g d") 'helm-gtags-dwim)
	    (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-find-pattern)
	    (define-key evil-normal-state-map (kbd "SPC g p") 'helm-gtags-find-pattern)
	    (define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-find-files)
	    (define-key evil-normal-state-map (kbd "SPC g f") 'helm-gtags-find-files)
	    (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
	    (define-key evil-normal-state-map (kbd "SPC g r") 'helm-gtags-find-rtag)
	    (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
	    (define-key evil-normal-state-map (kbd "SPC g s") 'helm-gtags-find-symbol)
	    (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
	    (define-key evil-normal-state-map (kbd "SPC g t") 'helm-gtags-find-tag)

	    (define-key evil-normal-state-map (kbd "SPC g R") 'rebuild-gtags-tags)

	    (defun helm-gtags-dwim-new-horizontal-split () (interactive)
		   (split-window-below)
		   (other-window 1)
		   (helm-gtags-dwim)
		   (recenter)
		   (other-window 1)
		   (recenter))
	    (define-key semantic-mode-map (kbd "C-c g 2 d") 'helm-gtags-dwim-new-horizontal-split)
	    (define-key evil-normal-state-map (kbd "SPC g 2 d") 'helm-gtags-dwim-new-horizontal-split)

	    (defun helm-gtags-dwim-new-vertical-split () (interactive)
		   (split-window-right)
		   (other-window 1)
		   (helm-gtags-dwim)
		   (recenter)
		   (other-window 1)
		   (recenter))
	    (define-key semantic-mode-map (kbd "C-c g 3 d") 'helm-gtags-dwim-new-vertical-split)
	    (define-key evil-normal-state-map (kbd "SPC g 3 d") 'helm-gtags-dwim-new-vertical-split)

	    ))

(defun delete-gtags-tags ()
  (interactive)
  ;; code ported from `helm-gtags-clear-cache'
  (let* ((tag-location (or helm-gtags--real-tag-location
			   helm-gtags--tag-location))
	 (gtags-path (concat tag-location "GTAGS"))
	 (grtags-path (concat tag-location "GRTAGS"))
	 (gpath-path (concat tag-location "GPATH")))
    (delete-file gtags-path)
    (delete-file grtags-path)
    (delete-file gpath-path)
    )
  )

(defun rebuild-gtags-tags ()
  (interactive)
  (let* ((tag-location (or helm-gtags--real-tag-location
			   helm-gtags--tag-location))
	 (tagroot tag-location)
	 (label "default")
	 (default-directory tagroot)
	 (label-opt (helm-gtags--label-option label)))
    (delete-gtags-tags)
    ;; code ported from `helm-gtags--find-tag-simple'
    (message "gtags is generating tags....")
    (if (zerop (process-file "gtags" nil nil nil "-q" label-opt))
      (message "gtags is generating tags.... done")
      (error "Failed: 'gtags -q %s'" label-opt)))
  )

(provide 'init-gtags)
