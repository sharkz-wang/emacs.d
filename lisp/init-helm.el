(require-package 'helm)
(helm-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC f o") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC \;") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "SPC \:") 'ibuffer)

(require 'helm-config)
(define-key evil-normal-state-map (kbd "SPC h") 'helm-command-prefix)
(define-key evil-normal-state-map (kbd "SPC h r") 'helm-resume)
(define-key evil-normal-state-map (kbd "SPC h b") 'helm-bookmark)
(define-key evil-normal-state-map (kbd "SPC t") 'helm-occur)

(require 'helm-files) ;; included in package helm
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") 'helm-find-files-down-last-level)

(require-package 'projectile)
(require-package 'helm-projectile)
(helm-projectile-on)

(projectile-global-mode 1)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

(define-key evil-normal-state-map (kbd "SPC \`") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p p") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p r") 'helm-projectile-recentf)
(define-key evil-normal-state-map (kbd "SPC p o") 'helm-projectile-find-other-file)
(define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)

(require-package 'helm-ag)
(define-key evil-normal-state-map (kbd "SPC p a") 'helm-projectile-ag)
(define-key evil-normal-state-map (kbd "SPC h a") 'helm-do-ag)

(provide 'init-helm)
