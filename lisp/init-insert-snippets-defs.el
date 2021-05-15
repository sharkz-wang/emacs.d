
(defun epxand-yasnippet-from-keyword (keyword)
  (interactive)
  (indent-according-to-mode)
  (evil-emacs-state)
  (yas-expand-snippet (yas-lookup-snippet keyword))
  )

(provide 'init-insert-snippets-defs)
