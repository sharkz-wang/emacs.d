
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

(setq org-link-named-repos '())

(defun org-link-open-named-repo (path)
    (message (format "%s" (split-string path ":")))
    ;; format: [repo:REPO_NAME:REL_PATH:LINE_NUM_OR_FUNC_NAME]
    ;; e.g.,   [repo:linux:kernel/sched/fair.c:6489]
    (let* ((str-list              (split-string path ":"))
           (repo-name             (nth 0 str-list))
           (rel-path              (nth 1 str-list))
           (line-num-or-func-name (nth 2 str-list))
           (repo-base-path (cdr (assoc repo-name org-link-named-repos))))
      (message (format "%s" repo-base-path))
      (find-file (concat (file-name-as-directory repo-base-path) rel-path))
      (if (= 0 (string-to-number line-num-or-func-name))
	  (progn
	    (goto-char (point-min))
	    (dumb-jump-go nil nil line-num-or-func-name))
	  (progn
	    (goto-char (point-min))
	    (forward-line (1- (string-to-number line-num-or-func-name))))
	  )
    )
)

(org-add-link-type "repo" 'org-link-open-named-repo)

(provide 'init-kernel-dev)
