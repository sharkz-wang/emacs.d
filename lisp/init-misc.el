;; Set default splitting approach to vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(evil-define-key 'normal image-mode-map (kbd "q") 'quit-window)

(provide 'init-misc)
