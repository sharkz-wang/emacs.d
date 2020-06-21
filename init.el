(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

(require 'init-package)

(require 'init-basics)
(require 'init-appearance)
(require 'init-helm)
(require 'init-ivy)
(require 'init-project)
(require 'init-dash)
(require 'init-magit)
(require 'init-search)
(require 'init-python)
(require 'init-c)
(require 'init-elisp)
(require 'init-org)
(require 'init-uml)
(require 'init-dired)
(require 'init-ediff)
(require 'init-ebook)
(require 'init-hex-file)
(require 'init-image)
(require 'init-kernel-dev)
(require 'init-help)
(require 'init-misc)

(require 'cust-vars)
