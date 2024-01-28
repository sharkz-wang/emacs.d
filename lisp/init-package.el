
(require 'init-straight)

(bootstrap-handler)

(defun require-package (package &optional min-version no-refresh)
  (--require-package package))

(require-package 'benchmark-init)

(customize-set-variable 'benchmark-init/list-sort-key '("total ms" . t))
(benchmark-init/activate)
(add-hook 'after-change-major-mode-hook 'benchmark-init/deactivate)

(provide 'init-package)
