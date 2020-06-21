(require-package 'projectile)
(require-package 'helm-projectile)

(helm-projectile-on)
(projectile-mode)

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)

(provide 'init-projectile)
