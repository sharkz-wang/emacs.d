;; TODO: load magit only since first time in git repo
(require-package 'magit)

;; set magit popup windows default to full-screen
(setq magit-display-buffer-function
	  #'magit-display-buffer-fullframe-status-v1)
;; explicitly set magit log date format
(setq magit-log-margin (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

(require-package 'evil-magit)
(evil-magit-init)

;; Unbind SPC - leading key of many useful key-bindings
(eval-after-load 'magit '(define-key magit-mode-map (kbd "SPC") nil))
(eval-after-load 'magit '(define-key magit-diff-mode-map (kbd "SPC") nil))
(eval-after-load 'magit '(define-key magit-blame-mode-map (kbd "SPC") nil))

(defun selectively-show-magit-diff (arg) (interactive "P")
  (if (equal current-prefix-arg '(4))
	(magit-diff-staged)
	(magit-diff-unstaged)
	)
  )

(define-key evil-normal-state-map (kbd "SPC m 1 TAB") 'magit-section-show-level-1-all)
(define-key evil-normal-state-map (kbd "SPC m 2 TAB") 'magit-section-show-level-2-all)
(define-key evil-normal-state-map (kbd "SPC m 3 TAB") 'magit-section-show-level-3-all)
(define-key evil-normal-state-map (kbd "SPC m 4 TAB") 'magit-section-show-level-4-all)

(define-key evil-normal-state-map (kbd "SPC m TAB") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC m s") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC m e") 'magit-ediff-stage)
(define-key evil-normal-state-map (kbd "SPC m $") 'magit-process-buffer)
(define-key evil-normal-state-map (kbd "SPC m B") 'magit-show-refs-popup)
(define-key evil-normal-state-map (kbd "SPC m d") 'selectively-show-magit-diff)
(define-key evil-normal-state-map (kbd "SPC m f d") 'magit-diff-buffer-file)
(define-key evil-normal-state-map (kbd "SPC m f b d") 'magit-ediff-compare)
(define-key evil-normal-state-map (kbd "SPC m D") 'magit-diff)
(define-key evil-normal-state-map (kbd "SPC m f l") 'magit-log-buffer-file)
(define-key evil-normal-state-map (kbd "SPC m b") 'magit-blame)
(define-key evil-normal-state-map (kbd "SPC m c") 'magit-commit)
(define-key evil-normal-state-map (kbd "SPC m r") 'magit-rebase-interactive)
(define-key evil-normal-state-map (kbd "SPC m a") 'magit-commit-amend)
(define-key evil-normal-state-map (kbd "SPC m i") 'magit-rebase-continue)
(define-key evil-normal-state-map (kbd "SPC m R") 'magit-ediff-resolve)
(define-key evil-normal-state-map (kbd "SPC m p") 'magit-push)
(define-key evil-normal-state-map (kbd "SPC m P") 'magit-pull)
(define-key evil-normal-state-map (kbd "SPC m l") 'magit-log-current)
(define-key evil-normal-state-map (kbd "SPC m L") 'magit-log)
(define-key evil-normal-state-map (kbd "SPC m C") 'magit-clean)
(define-key evil-normal-state-map (kbd "SPC m o") 'magit-checkout)
(define-key evil-normal-state-map (kbd "SPC m z") 'magit-stash)

(evil-define-key evil-magit-state magit-mode-map "=" 'magit-diff-less-context)
(define-key magit-log-mode-map (kbd "TAB") 'magit-cycle-margin-style)

(provide 'init-magit)
