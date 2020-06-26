(require-package 'helm)
(helm-mode 1)

(helm-autoresize-mode t)

(custom-set-variables
 '(helm-autoresize-max-height 30)
 '(helm-autoresize-min-height 30))

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'helm-config)

(add-hook 'helm-major-mode-hook
	  (lambda ()
	    (setq-local sublimity-attractive-centering-width 130)
	   ))

(evil-leader/set-key
  "bb" 'helm-buffers-list
  "rl" 'helm-resume
  "ry" 'helm-show-kill-ring
  )

(evil-global-set-key 'normal (kbd "SPC RET") 'helm-buffers-list)

(evil-leader/set-key
  "hl" 'helm-info-elisp
  )

(require 'cl)
;; a `helm-imenu' variation that won't take `thing-at-point' as default input
(defun helm-imenu-no-default ()
  (interactive)
  (cl-letf (((symbol-function 'thing-at-point)
	     #'(lambda (thing &optional no-properties) nil))
	    ;; XXX: 90 was the max valid number
	    (helm-autoresize-max-height 90)
	    (helm-autoresize-min-height 90)
	    )
    (helm-imenu)))

;; ditto `helm-imenu-in-all-buffers'
(defun helm-imenu-in-all-buffers-no-default ()
  (interactive)
  (cl-letf (((symbol-function 'thing-at-point)
	     #'(lambda (thing &optional no-properties) nil))
	    ;; XXX: 90 was the max valid number
	    (helm-autoresize-max-height 90)
	    (helm-autoresize-min-height 90)
	    )
    (helm-imenu-in-all-buffers)))

;; (evil-global-set-key 'normal (kbd "SPC s i") 'helm-imenu-no-default)
;; (evil-global-set-key 'normal (kbd "SPC s I") 'helm-imenu-in-all-buffers-no-default)

(defun helm-resize-buffer-to-max ()
     (interactive)
     ;; XXX: 90 was the max valid number
     (let ((helm-autoresize-max-height 90)
	   (helm-autoresize-min-height 90))
       (helm-refresh)))
(define-key helm-map (kbd "C-c C-m") 'helm-resize-buffer-to-max)

(require 'helm-files) ;; included in package helm
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-l") 'helm-find-files-down-last-level)

(require-package 'projectile)
(require-package 'helm-projectile)
(helm-projectile-on)

(projectile-global-mode 1)
(setq projectile-indexing-method 'hybrid)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

(define-key evil-normal-state-map (kbd "SPC \`") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p p") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p r") 'helm-projectile-recentf)
(define-key evil-normal-state-map (kbd "SPC p o") 'helm-projectile-find-other-file)
(define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)

(require-package 'helm-ag)
(define-key evil-normal-state-map (kbd "SPC h a") 'helm-do-ag)

(evil-define-key 'normal helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump)
(evil-define-key 'normal helm-ag-mode-map (kbd "q") 'quit-window)

(add-hook 'helm-ag-mode-hook
	  (lambda ()
	    (interactive)
	    (read-only-mode -1)
	   ))

(provide 'init-helm)
