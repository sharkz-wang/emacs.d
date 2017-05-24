(require 'init-helm)
(require-package 'helm-dash)

(setq browse-url-browser-function 'eww-browse-url)
(setq helm-dash-docsets-path (expand-file-name "docsets" user-emacs-directory))
(setq helm-dash-common-docsets '("C" "Python 2"))

(define-key evil-normal-state-map (kbd "SPC h d") 'helm-dash)

(provide 'init-dash)
