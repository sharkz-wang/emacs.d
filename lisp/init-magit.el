;; TODO: load magit only since first time in git repo
(require-package 'magit)
(require 'magit-log)

(custom-set-variables
 '(magit-log-arguments '("-n32" "--decorate")))

;; set magit popup windows default to full-screen
(setq magit-display-buffer-function
	  #'magit-display-buffer-fullframe-status-v1)
;; explicitly set magit log date format
(setq magit-log-margin (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

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

(require-package 'git-gutter)
(require-package 'fringe-helper)
(require 'git-gutter-fringe)

(global-git-gutter-mode 1)

;; TODO: move it to init-git.el
(require-package 'git-gutter)

(global-git-gutter-mode 1)

(setq git-gutter:ask-p nil)

(setq git-gutter:added-sign " + ")
(setq git-gutter:deleted-sign " - ")
(setq git-gutter:modified-sign " * ")

(evil-leader/set-key
  "g.." (lambda ()
	  (interactive)
	  (git-gutter:toggle)
	  (switch-to-last-buffer)
	  (switch-to-last-buffer)
	 )
  "g.s" 'git-gutter:stage-hunk
  "g.d" 'git-gutter:popup-diff
  "g.r" 'git-gutter:revert-hunk
  "g.n" 'git-gutter:next-hunk
  "g.j" 'git-gutter:next-hunk
  "g.p" 'git-gutter:previous-hunk
  "g.k" 'git-gutter:previous-hunk
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

(setq magit-status-sections-hook-orig
      magit-status-sections-hook)

;; XXX: by default C-u+`magit-status' shows list of known repos with hard-coded `basename',
;;      which was difficult to interpret.
;;      here we did the trick to show full path, by overriding
;;      the function `magit-repos-alist'.
(defun magit-repos-alist-full-path ()
   (--map (cons it it)
          (magit-list-repos)))

(defun magit-status-simplified ()
  (interactive)
  (setq magit-status-sections-hook
	'(magit-insert-untracked-files
	  magit-insert-unstaged-changes
	  magit-insert-staged-changes))
  (cl-letf (((symbol-function 'magit-repos-alist) 'magit-repos-alist-full-path))
    (call-interactively 'magit-status))
  (beginning-of-buffer)
  (magit-section-forward)
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
  (dired path)
  (magit-status-simplified))

(defun magit-status-full-on-path (path)
  (interactive)
  (dired path)
  (magit-status-full))

(evil-leader/set-key
  "gr" 'magit-diff-toggle-refine-hunk
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

(provide 'init-magit)
