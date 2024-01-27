;; TODO: load magit only since first time in git repo
(require-package 'magit)
(require 'magit-log)

;; default arguments for magit-log
(custom-set-variables
 '(magit-log-arguments '("-n32" "--decorate")))

;; set magit popup windows default to full-screen
(setq magit-display-buffer-function
	  #'magit-display-buffer-fullframe-status-v1)
;; explicitly set magit log date format
(setq magit-log-margin (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))
;; use date for the last time a commit is modified
(setq magit-log-margin-show-committer-date t)

;; customize ediff merge view into 3-col layout
(setq ediff-show-ancestor t)
(advice-add 'ediff-setup-windows-plain-merge :after
	    '--plain-merge-window-setup-3-col-layout)

;; require `cl-lib' to support following snippet
(require 'cl-lib)
(eval-after-load "projectile"
  '(progn
     (setq magit-repository-directories
	   (mapcar (lambda (dir)
		     (substring dir 0 -1))
		   (remove-if-not
		    (lambda (project)
		      (file-directory-p (concat project "/.git")))
		    (projectile-relevant-known-projects))))
     (setq magit-repository-directories-depth 1)
     )
  )

(defun helm-magit-dispatch-popup (arg)
  (interactive "P")
  (if (equal current-prefix-arg '(4))
      (let ((aux-buf-name (make-temp-name "magit-dispatch-popup-aux-"))
	    (repo-dir (helm :sources
			    (helm-build-sync-source "repositories"
			      :candidates (mapcar (lambda (x) (car x)) magit-repository-directories)))))
	(generate-new-buffer aux-buf-name)
	(with-current-buffer aux-buf-name
	  (message repo-dir)
	  (cd repo-dir)
	  (magit-dispatch-popup))
	;; (kill-buffer aux-buf-name)
	)
    (magit-dispatch-popup)
    ))

(eval-after-load "magit"
    '(progn
         (setq magit-status-sections-hook-orig
               magit-status-sections-hook)
     ))

;; XXX: by default C-u+`magit-status' shows list of known repos with hard-coded `basename',
;;      which was difficult to interpret.
;;      here we did the trick to show full path, by overriding
;;      the function `magit-repos-alist'.
(defun magit-repos-alist-full-path ()
   (--map (cons it it)
          (magit-list-repos)))

(defun --magit-get-section-count ()
  (length (oref magit-root-section children)))

(defun magit-status-simplified ()
  (interactive)
  (setq magit-status-sections-hook
	'(magit-insert-untracked-files
	  magit-insert-unstaged-changes
	  magit-insert-staged-changes))
  (cl-letf (((symbol-function 'magit-repos-alist) 'magit-repos-alist-full-path))
    (call-interactively 'magit-status))
  (beginning-of-buffer)
  (when (> (--magit-get-section-count) 0)
    (magit-section-forward))
 )

(defun magit-status-full ()
  (interactive)
  (setq magit-status-sections-hook
	magit-status-sections-hook-orig)
  (cl-letf (((symbol-function 'magit-repos-alist) 'magit-repos-alist-full-path))
    (call-interactively 'magit-status))
  (beginning-of-buffer)
  (magit-section-forward)
  (magit-section-forward)
  (magit-section-forward)
 )

(defun magit-dispatch-on-path (path)
  (interactive)
  (dired path)
  (magit-dispatch))

(defun magit-status-simplified-on-path (path)
  (interactive)
  (let ((default-directory path))
    (magit-status-simplified)))

(defun magit-status-full-on-path (path)
  (interactive)
  (let ((default-directory path))
    (magit-status-full)))

(evil-leader/set-key
  "g$" 'magit-process-buffer
  "g4" 'magit-process-buffer
  )

(defun magit-section-forward-scroll-to-top ()
  (interactive)
  (magit-section-forward)
  (evil-scroll-line-to-top (line-number-at-pos))
    )

(defun magit-section-backward-scroll-to-top ()
  (interactive)
  (magit-section-backward)
  (evil-scroll-line-to-top (line-number-at-pos))
    )

(defun magit-section-forward-sibling-scroll-to-top ()
  (interactive)
  (magit-section-forward-sibling)
  (evil-scroll-line-to-top (line-number-at-pos))
    )

(defun magit-section-backward-sibling-scroll-to-top ()
  (interactive)
  (magit-section-backward-sibling)
  (evil-scroll-line-to-top (line-number-at-pos))
    )

(setq magit-section-cycle-level 2)
(defun magit-section-cycle-show-level-all ()
  (interactive)
  (if (eq magit-section-cycle-level 2)
      (progn
	(magit-section-show-level-4-all)
	(setq magit-section-cycle-level 4)
	)
    (progn
      (magit-section-show-level-2-all)
      (setq magit-section-cycle-level 2)
      )))

(defun projectile-git-repo-list ()
  (remove-if-not
   (lambda (project)
     (file-directory-p (concat project "/.git/")))
   (projectile-relevant-known-projects)))

(eval-after-load "projectile"
  '(progn (setq magit-repository-directories
		(mapcar (lambda (dir) (cons dir 0))
		(projectile-git-repo-list)))))

;; make following buffers always shown in full screen window
(add-to-list 'display-buffer-alist
	     ;; diff buffers (both staged/unstaged)
	     '(("magit-diff: .*" display-buffer-same-window))
	     t)

;; by default, don't display the slow diff view when editing commit message
(customize-set-variable 'magit-commit-show-diff nil)

(evil-define-key 'normal
  magit-diff-mode-map (kbd "m") 'evil-set-marker-local-global)
(evil-define-key 'normal
  magit-diff-mode-map (kbd "'") 'evil-goto-global-mark-line)
(evil-define-key 'normal
  magit-status-mode-map (kbd "m") 'evil-set-marker-local-global)
(evil-define-key 'normal
  magit-status-mode-map (kbd "'") 'evil-goto-global-mark-line)
(evil-define-key 'normal
  magit-diff-mode-map (kbd "SPC g r") 'magit-diff-toggle-refine-hunk)

(provide 'init-magit)
