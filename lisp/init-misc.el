(require-package 'ztree)
(define-key evil-normal-state-map (kbd "SPC f d d") 'ztree-diff)

(eval-after-load "ztree"
  '(progn
     (evil-define-key 'motion ztree-mode-map (kbd "TAB") 'ztree-perform-soft-action)
     (evil-define-key 'motion ztree-mode-map (kbd "RET") 'ztree-perform-action)
     (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-jump-side)
     (evil-define-key 'motion ztree-mode-map (kbd "l") 'ztree-jump-side)
     (evil-define-key 'motion ztree-mode-map (kbd "r") 'ztree-diff-partial-rescan)
     (evil-define-key 'motion ztree-mode-map (kbd "R") 'ztree-diff-full-rescan)
     (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-diff-toggle-show-equal-files)
     (evil-define-key 'motion ztree-mode-map (kbd "v") 'ztree-diff-view-file)
     (evil-define-key 'motion ztree-mode-map (kbd "o") 'ztree-diff-view-file)
     (evil-define-key 'motion ztree-mode-map (kbd "q") 'quit-window)
     ))

(provide 'init-misc)
