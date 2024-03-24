(require 'init-python-defs)

(add-hook 'python-mode-hook 'init-python-mode)
(defun init-python-mode ()

  (require-package 'evil-indent-textobject)

  ;; setup major-mode interface functions
  (setq insert-for-loop 'python-insert-for-loop)
  (setq insert-print 'python-insert-print)

  (setq insert-todo-comment 'python-insert-ptyhon-todo-comment)
  (setq insert-fixme-comment 'python-insert-ptyhon-fixme-comment)
  (setq insert-xxx-comment 'python-insert-ptyhon-xxx-comment)

  (setq append-argument 'python-insert-new-arg)
  (setq insert-argument-select 'python-avy-insert-new-arg)
  ;; end major-mode interface functions
  )

(provide 'init-python)
