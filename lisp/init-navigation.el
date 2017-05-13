(require-package 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-char-2)

(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(define-key evil-normal-state-map (kbd "SPC w") 'ace-window)

(require-package 'smooth-scrolling)
(smooth-scrolling-mode 1)

(require-package 'sublimity)
(require 'sublimity-scroll)
(sublimity-mode 1)

(provide 'init-navigation)
