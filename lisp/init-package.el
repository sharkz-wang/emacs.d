(require 'package)
(package-initialize)

(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))

(setq package-archive-priorities '(("gnu"     . 5)
				   ("melpa"  . 0)))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (message "Checking package `%s'" package)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'init-package)
