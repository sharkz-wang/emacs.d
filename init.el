(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

(require 'init-basics)
(require 'init-appearance)
(require 'init-helm)
(require 'init-magit)
(require 'init-company)
(require 'init-python)
