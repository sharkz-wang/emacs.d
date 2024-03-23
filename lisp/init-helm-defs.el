(require-package 'cl-lib)

(defun --helm-show-kill-ring-short ()
    (interactive)
    (customize-set-variable 'helm-kill-ring-max-offset 30)
    (helm-show-kill-ring))

(defun --helm-show-kill-ring-long ()
    (interactive)
    (customize-set-variable 'helm-kill-ring-max-offset 400)
    (helm-show-kill-ring))

(defun --helm-resume-select ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'helm-resume)
    ))

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

(defun helm-toggle-resize-buffer-to-max ()
  (interactive)
  (if (and (cl-equalp curr-helm-max-height orig-helm-max-height)
	   (cl-equalp curr-helm-min-height orig-helm-min-height))
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

(setq --helm-minibuffer-content "")
(defun --helm-save-search-session ()
  (interactive)
  (let ((new-buf-name (format "%s: %s"
			      (s-replace "*" "" (helm-buffer-get))
			      --helm-minibuffer-content)))
    (add-to-list 'helm-buffers (list new-buf-name))
    )
  )

(defun helm-save-search-session ()
  (interactive)
  (setq --helm-minibuffer-content (minibuffer-contents-no-properties))
  (helm-run-after-quit '--helm-save-search-session)
  )

(defun helm-goto-line-ow-forward ()
  (interactive)
  (helm-next-line)
  (helm-execute-persistent-action))

(defun helm-goto-line-ow-backward ()
  (interactive)
  (helm-previous-line)
  (helm-execute-persistent-action))

(defun --do-helm-ag-copy-line (candidate)
  ;; candidate format example:
  ;;         "init-c.el:20:(defun c-insert-print ()"
  (kill-new (nth 2 (split-string candidate ":")))
  )

(defun --helm-ag-copy-line ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'--do-helm-ag-copy-line))
  )

(defun --helm-open-magit-here (candidate)
  (magit-status-simplified-on-path
   (file-name-parent-directory candidate)))

(helm-make-command-from-action --do-helm-open-magit-here
  "Not documented." '--helm-open-magit-here)

(defun --helm-projectile-find-file-here (candidate)
  ;; XXX: don't use `helm-projectile-find-file', it pops annoying
  ;;      message about clearing up recentf list
  (let ((default-directory candidate))
      (projectile-find-file)))

(helm-make-command-from-action --do-helm-projectile-find-file-here
  "Not documented." '--helm-projectile-find-file-here)

(provide 'init-helm-defs)
