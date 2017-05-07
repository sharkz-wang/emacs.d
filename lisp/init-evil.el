(require-package 'evil)
(evil-mode t)

;; set starting state to normal state
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)
(setq evil-default-state 'normal)
;; end setting starting state

(require-package 'evil-surround)
(global-evil-surround-mode t)

(require-package 'evil-snipe)
(evil-snipe-mode t)

(setq evil-snipe-repeat-keys nil)
(setq evil-snipe-smart-case t)

;; globally set alias symbols, can be extended in minor modes
(setq evil-snipe-aliases nil)
(push '(?\[ "[[{(]") evil-snipe-aliases)
(push '(?\] "[]})]") evil-snipe-aliases)
(push '(?' "[\"']") evil-snipe-aliases)

;; fix conflict with Magit according to user manual
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

(require-package 'evil-matchit)
(global-evil-matchit-mode t)

(provide 'init-evil)
