(require-package 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "SPC '") 'avy-goto-char-2)

(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(define-key evil-normal-state-map (kbd "SPC w") 'ace-window)

(provide 'init-navigation)
