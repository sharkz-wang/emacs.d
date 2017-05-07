;; skip welcome screen
(setq inhibit-splash-screen t)
;; display column numbers in mode-line
(column-number-mode 1)
(set-default-font "Deja Vu Sans Mono-16")
;; no tool bars
(tool-bar-mode -1)
;; no memu bar
(menu-bar-mode -1)

;; set left margin size and make it take effect now
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(set-window-margins
			  (car (get-buffer-window-list (current-buffer) nil t))
			  3)))

(require-package 'undo-tree)
(global-undo-tree-mode 1)

(require 'init-evil)
(require 'init-navigation)

(provide 'init-basics)
