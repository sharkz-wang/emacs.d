
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

(provide 'init-magit-defs)
