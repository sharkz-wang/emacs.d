(require-package 'projectile)
(require-package 'helm-projectile)

(helm-projectile-on)
(projectile-mode)

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)

(define-key evil-normal-state-map (kbd "SPC \`") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p p") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p r") 'helm-projectile-recentf)
(define-key evil-normal-state-map (kbd "SPC p o") 'helm-projectile-find-other-file)

(provide 'init-projectile)
