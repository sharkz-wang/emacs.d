
(defun kernel-lxr-link-open (path)
  (let ((rel-path (car (split-string path "::")))
	(line-num (car (cdr (split-string path "::")))))
    (browse-url (format
		 "https://elixir.bootlin.com/linux/v4.9.92/source/%s#L%s"
		 rel-path
		 line-num))
    )
  )

(org-add-link-type "kernel-lxr" 'kernel-lxr-link-open)

(defcustom kernel-src-repo-dir ""
  "Kernel source repo directory path."
  :type 'string
  :group 'init-org)

(defcustom kernel-src-repo-list '()
  "Kernel source repo list."
  :type '(repeat string)
  :group 'init-org)

(defun switch-kernel-src-repo ()
  (interactive)
  (customize-set-variable 'kernel-src-repo-dir
			  (helm :sources
				(helm-build-sync-source "Select kernel src repo"
				  :candidates kernel-src-repo-list)))
  )

(defun kernel-src-link-open (path)
  (let ((rel-path (car (split-string path "::")))
	(line-num (car (cdr (split-string path "::")))))
    (find-file (concat (file-name-as-directory kernel-src-repo-dir) rel-path))
    (goto-line (string-to-number line-num))
    )
  )

(org-add-link-type "kernel-src" 'kernel-src-link-open)

(provide 'init-kernel-dev)
