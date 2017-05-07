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

(require 'init-evil)

;; misc ugly but useful key-bindings
(define-key evil-normal-state-map (kbd "SPC s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC j") 'switch-to-last-buffer)

(defun switch-to-last-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x l") 'switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "SPC l") (lambda () (interactive)
						  (flop-frame)
						  (other-window 1)))

(require 'init-navigation)

(require-package 'undo-tree)
(global-undo-tree-mode 1)

;; settings for smex
(require-package 'ido)
(ido-mode t)
(setq ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

(require-package 'smex)
(add-hook 'ido-setup-hook
		  (lambda ()
			(add-to-list 'load-path (expand-file-name ".smex-items" user-emacs-directory))

			(define-key ido-completion-map (kbd "C-j") 'ido-next-match)
			(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
			(define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
			(define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
			))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; end settings for smex

(provide 'init-basics)
