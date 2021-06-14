;; Set default splitting approach to vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun copy-file-location ()
  (interactive)
  (kill-new (format "%s" (buffer-file-name) (line-number-at-pos))))

(define-key evil-normal-state-map (kbd "SPC t p") 'copy-file-location)

(evil-define-key 'normal image-mode-map (kbd "q") 'quit-window)

(provide 'init-misc)
