(require 'init-helm)
(require-package 'helm-dash)

(setq helm-dash-docsets-path (expand-file-name "docsets" user-emacs-directory))
(setq helm-dash-common-docsets '("C" "Python 2"))

(require 'init-eww)

(define-key evil-normal-state-map (kbd "SPC h D")
  (lambda ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (helm-dash))))

(provide 'init-dash)
