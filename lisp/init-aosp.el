(defun file-lines-to-list (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun aosp-manifest-repo-path-list (manifest-f)
  (-non-nil (mapcar
	     (lambda (item)
	       (when (string-match "[[:blank:]]*<project[[:blank:]]+path=[\"']\\(.*?\\)[\"']" item)
		 (match-string-no-properties 1 item)))
	     (split-string (file-lines-to-list manifest-f) "\n" t)))
  )

(defun aosp-manifest-projectile-choose-repo (manifest-f)
  (interactive)
  (let ((default-directory
	  (helm :sources
		(helm-build-sync-source "Repo list"
		  :candidates
		  (aosp-manifest-repo-path-list manifest-f)))))
    (helm-projectile-find-file)
    ))

(provide 'init-aosp)
