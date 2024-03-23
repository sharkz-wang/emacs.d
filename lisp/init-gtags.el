(require-package 'ggtags)
(require-package 'helm-gtags)

(require 'init-search-defs)
(require 'init-gtags-defs)

;; Making GNU Global support more languages
;; 1) Install Exuberant Ctags
;; 2) Run `pip install pygments`
;; 3) Copy /usr/local/share/gtags/gtags.conf to ~/.globalrc
;; 4) Update ~/.globalrc: change pigments-parser.la and exuberant-ctags.la to *.so and correct their path
;; 5) Build tags by running `gtags --gtagslabel=pygments`

(helm-gtags-mode 1)

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-suggested-key-mapping t)

(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d R") 'rebuild-gtags-tags)

(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d S") 'helm-gtags-select)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d d") 'helm-gtags-dwim)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d j") 'helm-gtags-dwim)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d p") 'helm-gtags-find-pattern)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d f") 'helm-gtags-find-files)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d r") 'helm-gtags-find-rtag)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d s") 'helm-gtags-find-tag)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d 2 d") 'helm-gtags-dwim-new-horizontal-split)
(evil-define-minor-mode-key 'normal 'helm-gtags-mode-map (kbd "SPC d 3 d") 'helm-gtags-dwim-new-vertical-split)

(advice-add 'helm-gtags-dwim :before #'push-current-mark)
(advice-add 'helm-gtags-select :before #'push-current-mark)
(advice-add 'helm-gtags-find-pattern :before #'push-current-mark)
(advice-add 'helm-gtags-find-files :before #'push-current-mark)
(advice-add 'helm-gtags-find-rtag :before #'push-current-mark)
(advice-add 'helm-gtags-find-symbol :before #'push-current-mark)
(advice-add 'helm-gtags-find-tag :before #'push-current-mark)
(advice-add 'helm-gtags-dwim-new-horizontal-split :before #'push-current-mark)
(advice-add 'helm-gtags-dwim-new-vertical-split :before #'push-current-mark)

(provide 'init-gtags)
