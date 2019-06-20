(require-package 'avy)

(evil-leader/set-key
  "j" 'avy-goto-char
  )

(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; (define-key evil-normal-state-map (kbd "SPC w") 'ace-window)

(require-package 'vimish-fold)

(defun vimish-action (arg) (interactive "P")
       (if (equal current-prefix-arg '(4))
	   (vimish-fold-delete)
	 (vimish-fold-toggle)
	 ))

(define-key evil-normal-state-map (kbd "S-TAB") 'vimish-action)
(define-key evil-normal-state-map (kbd "<backtab>") 'vimish-action)

(define-key evil-visual-state-map (kbd "S-TAB") 'vimish-fold)
(define-key evil-visual-state-map (kbd "<backtab>") 'vimish-fold)

(provide 'init-navigation)
