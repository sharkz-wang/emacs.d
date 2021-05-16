
(defun git-get-file-location-info ()
  "Returns '(current's full-hash, relative file name,
   current line number)"
  (list (magit-rev-parse "HEAD")
	(file-relative-name (buffer-file-name)
			    (magit-toplevel))
	(line-number-at-pos))
)

(provide 'init-magit-defs)
