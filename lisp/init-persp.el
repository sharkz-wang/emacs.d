(require-package 'persp-mode)

(persp-mode 1)

(evil-global-set-key 'normal (kbd "SPC a j") 'persp-next)
(evil-global-set-key 'normal (kbd "SPC a k") 'persp-prev)
(evil-global-set-key 'normal (kbd "SPC a n") 'persp-add-new)
(evil-global-set-key 'normal (kbd "SPC a K") 'persp-kill)

(evil-global-set-key 'normal (kbd "SPC a a") 'persp-switch)
(evil-global-set-key 'normal (kbd "SPC a RET") 'persp-switch-to-buffer)
(evil-global-set-key 'normal (kbd "SPC a +") 'persp-add-buffer)

(provide 'init-persp)
