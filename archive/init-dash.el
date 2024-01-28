(require 'init-helm)
(require-package 'helm-dash)

(setq helm-dash-docsets-path (expand-file-name "docsets" user-emacs-directory))
(setq helm-dash-common-docsets '("C" "Python 2"))

(require 'init-eww)

;; TODO: add helm-dash

(provide 'init-dash)
