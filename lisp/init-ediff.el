(defun ediff-copy-both-to-C (head buttom)
  (interactive)
  (ediff-copy-diff
      ediff-current-difference
      nil
      'C
      nil
      (concat
          (ediff-get-region-contents
              ediff-current-difference
              head
              ediff-control-buffer)
          (ediff-get-region-contents
              ediff-current-difference
              buttom
              ediff-control-buffer))))


;; TODO: fix following failed key-binding
;; error message:
;;     init-ediff: Key sequence "d a" starts with non-prefix key d
;; code:
;;     (define-key ediff-mode-map
;;                 (kbd "d a")
;;                 (lambda ()
;;                 (interactive)
;;                 (ediff-copy-both-to-C 'A 'B)))
;;     (add-hook 'ediff-keymap-setup-hook 'init-ediff)

(defun ediff-copy-A-B-to-C ()
    (interactive)
    (ediff-copy-both-to-C 'A 'B)
)

(defun ediff-copy-B-A-to-C ()
    (interactive)
    (ediff-copy-both-to-C 'B 'A)
)

(provide 'init-ediff)
