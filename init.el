(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

(require 'cust-vars)

(require 'init-package)

(require 'init-basics)
(require 'init-appearance)
(require 'init-helm)
(require 'init-dash)
(require 'init-magit)
(require 'init-search)
(require 'init-python)
(require 'init-c)
(require 'init-elisp)
(require 'init-org)
(require 'init-uml)
(require 'init-dired)
(require 'init-ebook)
(require 'init-hex-file)
(require 'init-misc)

