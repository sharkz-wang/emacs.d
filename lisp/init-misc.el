(require-package 'compile)
(setq compilation-scroll-output t)
;; Set default splitting approach to vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(require-package 'ztree)
(define-key evil-normal-state-map (kbd "SPC f d d") 'ztree-diff)

(add-hook 'ztree-mode-hook
	  (lambda ()
	    (interactive)
	    ;; ztree-mode is default to `evil-motion-state' (which is defined in init-evil.el)
	    (evil-define-key 'motion ztree-mode-map (kbd "TAB") 'ztree-perform-soft-action)
	    (evil-define-key 'motion ztree-mode-map (kbd "RET") 'ztree-perform-action)
	    (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-jump-side)
	    (evil-define-key 'motion ztree-mode-map (kbd "l") 'ztree-jump-side)
	    (evil-define-key 'motion ztree-mode-map (kbd "r") 'ztree-diff-partial-rescan)
	    (evil-define-key 'motion ztree-mode-map (kbd "R") 'ztree-diff-full-rescan)
	    (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-diff-toggle-show-equal-files)
	    (evil-define-key 'motion ztree-mode-map (kbd "v") 'ztree-diff-view-file)
	    (evil-define-key 'motion ztree-mode-map (kbd "o") 'ztree-diff-view-file)
	    (evil-define-key 'motion ztree-mode-map (kbd "q") 'quit-window)
     ))

(defun copy-file-location ()
  (interactive)
  (kill-new (format "%s:%d" (buffer-file-name) (line-number-at-pos))))

(define-key evil-normal-state-map (kbd "SPC t p") 'copy-file-location)

(evil-define-key 'normal image-mode-map (kbd "q") 'quit-window)

(provide 'init-misc)
