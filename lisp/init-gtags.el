(require-package 'ggtags)
(require-package 'helm-gtags)

;; Making GNU Global support more languages
;; 1) Install Exuberant Ctags
;; 2) Run `pip install pygments`
;; 3) Copy /usr/local/share/gtags/gtags.conf to ~/.globalrc
;; 4) Update ~/.globalrc: change pigments-parser.la and exuberant-ctags.la to *.so and correct their path
;; 5) Build tags by running `gtags --gtagslabel=pygments`

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(defun push-current-mark (&rest r)
     (push-mark))

(add-hook
 'helm-gtags-mode-hook
 (lambda ()

   (setq helm-gtags-ignore-case t
	 helm-gtags-auto-update t
	 helm-gtags-use-input-at-cursor t
	 helm-gtags-pulse-at-cursor t
	 helm-gtags-suggested-key-mapping t)

   (define-key evil-normal-state-map (kbd "SPC d S") 'helm-gtags-select)
   (define-key evil-normal-state-map (kbd "SPC d d") 'helm-gtags-dwim)
   (define-key evil-normal-state-map (kbd "SPC d p") 'helm-gtags-find-pattern)
   (define-key evil-normal-state-map (kbd "SPC d f") 'helm-gtags-find-files)
   (define-key evil-normal-state-map (kbd "SPC d r") 'helm-gtags-find-rtag)
   (define-key evil-normal-state-map (kbd "SPC d s") 'helm-gtags-find-symbol)
   (define-key evil-normal-state-map (kbd "SPC d t") 'helm-gtags-find-tag)
   (define-key evil-normal-state-map (kbd "SPC d 2 d") 'helm-gtags-dwim-new-horizontal-split)
   (define-key evil-normal-state-map (kbd "SPC d 3 d") 'helm-gtags-dwim-new-vertical-split)

   (advice-add 'helm-gtags-dwim :before #'push-current-mark)
   (advice-add 'helm-gtags-select :before #'push-current-mark)
   (advice-add 'helm-gtags-find-pattern :before #'push-current-mark)
   (advice-add 'helm-gtags-find-files :before #'push-current-mark)
   (advice-add 'helm-gtags-find-rtag :before #'push-current-mark)
   (advice-add 'helm-gtags-find-symbol :before #'push-current-mark)
   (advice-add 'helm-gtags-find-tag :before #'push-current-mark)
   (advice-add 'helm-gtags-dwim-new-horizontal-split :before #'push-current-mark)
   (advice-add 'helm-gtags-dwim-new-vertical-split :before #'push-current-mark)
   ))

(provide 'init-gtags)
