(require-package 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "SPC '") 'avy-goto-char-2)

(provide 'init-navigation)
