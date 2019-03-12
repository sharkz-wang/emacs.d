(require 'eww)

;; use font-enabled layouts
(setq shr-use-fonts nil)

(evil-define-key 'normal eww-mode-map "q" 'quit-window)
(evil-define-key 'normal eww-mode-map "m" 'evil-set-marker-local-global)
(evil-define-key 'normal eww-mode-map "'" 'evil-goto-global-mark-line)

(provide 'init-eww)
