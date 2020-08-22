(require-package 'helm)
(helm-mode 1)

(helm-autoresize-mode t)

(custom-set-variables
 '(helm-autoresize-max-height 30)
 '(helm-autoresize-min-height 30))
(setq orig-helm-max-height helm-autoresize-max-height)
(setq orig-helm-min-height helm-autoresize-min-height)
(setq curr-helm-max-height helm-autoresize-max-height)
(setq curr-helm-min-height helm-autoresize-min-height)

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

;; XXX: in `helm-do-ag', simply using `let' won't work, as minibuffer would still be
;;      updated after `helm-refresh'
(add-hook 'helm-quit-hook
	  (lambda ()
	    (setq helm-autoresize-max-height curr-helm-max-height)
	    (setq helm-autoresize-min-height curr-helm-min-height)))

(defun helm-toggle-resize-buffer-to-max ()
  (interactive)
  (if (and (equalp curr-helm-max-height orig-helm-max-height)
	   (equalp curr-helm-min-height orig-helm-min-height))
      (progn
	;; XXX: 90 was the max valid number
	(setq curr-helm-max-height 90)
	(setq curr-helm-min-height 90))
    (progn
      (setq curr-helm-max-height orig-helm-max-height)
      (setq curr-helm-min-height orig-helm-min-height)
      ))
  (setq helm-autoresize-max-height curr-helm-max-height)
  (setq helm-autoresize-min-height curr-helm-min-height)
  (helm-refresh))

(define-key helm-map (kbd "C-c C-m") 'helm-toggle-resize-buffer-to-max)

(defun --helm-save-search-session ()
  (interactive)
  (let* ((buf-name (helm-buffer-get))
	 (new-buf-name (helm-buffers-rename-buffer buf-name)))
    (setq helm-buffers (append helm-buffers (list new-buf-name)))
    )
  )
(defun helm-save-search-session ()
  (interactive)
  (helm-run-after-quit '--helm-save-search-session)
  )

(define-key helm-map (kbd "C-c C-s") helm-save-search-session)

(require 'helm-files) ;; included in package helm
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") 'helm-find-files-down-last-level)

(defun helm-goto-line-ow-forward ()
  (interactive)
  (helm-next-line)
  (helm-execute-persistent-action))

(defun helm-goto-line-ow-backward ()
  (interactive)
  (helm-previous-line)
  (helm-execute-persistent-action))

(define-key helm-map (kbd "C-M-j") 'helm-goto-line-ow-forward)
(define-key helm-map (kbd "C-M-k") 'helm-goto-line-ow-backward)
(define-key helm-moccur-mode-map (kbd "C-M-j") 'helm-moccur-mode-goto-line-ow-forward)
(define-key helm-moccur-mode-map (kbd "C-M-k") 'helm-moccur-mode-goto-line-ow-backward)

(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-l") 'helm-find-files-down-last-level)

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
