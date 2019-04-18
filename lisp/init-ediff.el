(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference
			       'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference
			       'B ediff-control-buffer))))

(defun init-ediff ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)
  )

(add-hook 'ediff-keymap-setup-hook 'init-ediff)

(provide 'init-ediff)
