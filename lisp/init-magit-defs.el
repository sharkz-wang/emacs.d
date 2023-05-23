
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

(provide 'init-magit-defs)
