(require-package 'treemacs)

(setq treemacs-width 35)
;; exclude treemacs window from window movement commands
(setq treemacs-is-never-other-window t)

(add-hook 'treemacs-mode-hook
	  (lambda ()
	    (treemacs-follow-mode 1)
	    (display-line-numbers-mode -1)
	    ))

(evil-global-set-key 'normal (kbd "SPC t f") 'treemacs)

(evil-define-key 'normal treemacs-mode-map (kbd "RET") 'treemacs-RET-action)
(evil-define-key 'normal treemacs-mode-map (kbd "TAB") 'treemacs-toggle-node)
(evil-define-key 'normal treemacs-mode-map (kbd ".")   'treemacs-toggle-show-dotfiles)
(evil-define-key 'normal treemacs-mode-map (kbd "P")   'treemacs-peek-mode)

(provide 'init-treemacs)
