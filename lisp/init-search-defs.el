
(defun push-current-mark (&rest r)
  (evil--jumps-push) (push-mark))

(provide 'init-search-defs)
