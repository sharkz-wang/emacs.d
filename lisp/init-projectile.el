(require-package 'projectile)
(require-package 'helm-projectile)

(helm-projectile-on)
(projectile-mode)

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)

;; remove annoying output buffers, e.g., "*Help*",
;; from projectile's buffer list
(defun --filter-out-output-buffers (buf-list)
  (seq-filter (lambda (buf-name)
                (not (string-prefix-p "*" buf-name)))
              buf-list)
)
(advice-add 'projectile-project-buffer-names
            :filter-return #'--filter-out-output-buffers)

(provide 'init-projectile)
