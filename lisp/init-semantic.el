(require 'semantic)
(semantic-mode 1)
(global-semantic-stickyfunc-mode 1)

(evil-define-key 'normal semantic-mode-map (kbd "SPC g i") 'semantic-ia-show-summary)

(provide 'init-semantic)
