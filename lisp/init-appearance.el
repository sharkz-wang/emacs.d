(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require-package 'monokai-theme)
(load-theme 'monokai t)

(require-package 'moe-theme)
(require 'moe-theme)

(require-package 'zone-rainbow)

(require-package 'nyan-mode)
(nyan-mode 1)

(provide 'init-appearance)
