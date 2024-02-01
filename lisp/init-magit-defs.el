
;; XXX: by default C-u+`magit-status' shows list of known repos with hard-coded `basename',
;;      which was difficult to interpret.
;;      here we did the trick to show full path, by overriding
;;      the function `magit-repos-alist'.
(defun magit-repos-alist-full-path ()
   (--map (cons it it) (magit-list-repos)))

(defun git-get-file-location-info ()
  "Returns current file's ...
       '(full revision hash,
         short revision hash,
         relative file name,
         line number)"
  (list (magit-rev-parse "HEAD")
        (magit-rev-parse "--short" "HEAD")
        (file-relative-name (buffer-file-name)
                            (projectile-project-root))
        (line-number-at-pos))
)

(defun magit-status-simplified-on-path (path)
  (interactive)
  (let ((default-directory path))
    (magit-status-simplified)))

(defun projectile-git-repo-list ()
  (cl-remove-if-not
   (lambda (project)
     (file-directory-p (concat project "/.git/")))
   (projectile-relevant-known-projects)))

(defun magit-quick-stash-all ()
  (interactive)

  ;; save all buffers before stashing
  (projectile-save-project-buffers)

  (magit-log-head '("--decorate" "-n5"))
  ;; don't make subsequent calls instantly take over magit-log buffer
  (sit-for 1)
  (magit-run-git-with-editor "commit" "--all"
			     "-m" "wip: stash commit")
  (magit-log-head '("--decorate" "-n5"))
)

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

(defun --plain-merge-window-setup-3-col-layout (buf-A buf-B buf-C
						      control-buffer)
  "Transform plain merge window to 3-column layout
(with 4-th window for common ancestor)."
  (let ((buf-A        (window-buffer ediff-window-A))
	(buf-B        (window-buffer ediff-window-B))
	(buf-C        (window-buffer ediff-window-C))
	(buf-Ancestor (window-buffer ediff-window-Ancestor)))

    ;; remove window dedication before setting up multi-window layout
    (set-window-dedicated-p (get-buffer-window control-buffer) nil)

    (delete-other-windows)
    (split-window-vertically) (other-window 1)

    ;; start splitting up windows
    ;;
    ;; |          |                      |          |
    ;; | buffer-A | buffer C (for merge) | buffer B |
    ;; |          |                      |          |
    ;; |          +----------------------+          |
    ;; |          | buffer ancestor      |          |
    ;; |          |                      |          |

    (switch-to-buffer buf-A)
    (split-window-horizontally) (switch-to-buffer buf-C)
    (split-window-horizontally) (switch-to-buffer buf-B)

    ;; make 3 columns equally size
    (balance-windows)

    (when (and ediff-show-ancestor buf-Ancestor)
      (select-window (get-buffer-window buf-C))
      (split-window-vertically) (switch-to-buffer buf-Ancestor))
    ;; end window setup

    ;; reset control buffer state, these two lines are mandatory
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)

    ;; export variables again
    (with-current-buffer control-buffer
      (setq ediff-window-A (get-buffer-window buf-A)
	    ediff-window-B (get-buffer-window buf-B)
	    ediff-window-C (get-buffer-window buf-C)
	    ediff-window-Ancestor (get-buffer-window buf-Ancestor)))
    ))

(defun magit-open-known-project()
  (interactive)
  (let ((default-directory
	 (expand-file-name
	  (helm :sources
		(helm-build-sync-source "Select project:"
		  :candidates (projectile-relevant-known-projects))))))
    (call-interactively 'magit-status-simplified))
  )

(provide 'init-magit-defs)
