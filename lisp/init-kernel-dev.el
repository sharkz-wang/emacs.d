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

(define-key teleport-map "kk"
	    (lambda () (interactive) (teleport-do-action kernel-src-repo-dir)))
(define-key teleport-map "ki"
	    (lambda () (interactive) (teleport-do-action (concat-path kernel-src-repo-dir "include"))))
(define-key teleport-map "kd"
	    (lambda () (interactive) (teleport-do-action (concat-path kernel-src-repo-dir "Documentation"))))

(provide 'init-kernel-dev)
