;; skip welcome screen
(setq inhibit-splash-screen t)
;; display column numbers in mode-line
(column-number-mode 1)
(set-default-font "Monaco-16")
;; no tool bars
(tool-bar-mode -1)
;; no memu bar
(menu-bar-mode -1)
;; no scroll bars
(scroll-bar-mode -1)

;; always use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; set left margin size and make it take effect now
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(set-window-margins
			  (car (get-buffer-window-list (current-buffer) nil t))
			  3)))

;; make emacs know last opened position of a file
(if (< emacs-major-version 25)
    (progn
      (require-package 'saveplace)
      (setq-default save-place t))
  (progn
    (save-place-mode 1))
  )
(setq save-place-file (expand-file-name ".saveplace" user-emacs-directory))

(require 'init-evil)

(define-key evil-normal-state-map
  (kbd "SPC TAB")
  (lambda (arg) (interactive "P")
    (if (equal current-prefix-arg '(4))
	(progn
	  (set-frame-parameter nil 'fullscreen 'maximized)
	)
      (set-frame-parameter nil 'fullscreen 'fullboth)
      )
    ))

(defun switch-to-last-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; misc ugly but useful key-bindings
(define-key evil-normal-state-map (kbd "SPC u") 'universal-argument)
(define-key evil-normal-state-map (kbd "SPC s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC j") 'switch-to-last-buffer)

(define-key evil-normal-state-map (kbd "SPC l") (lambda ()
						  (interactive)
						  (kill-buffer (current-buffer))))

(define-key evil-normal-state-map (kbd "SPC 0") 'delete-window)
(define-key evil-normal-state-map (kbd "SPC 1") 'delete-other-windows)

(define-key evil-normal-state-map (kbd "SPC 2") (lambda ()
						  (interactive)
						  (split-window-below)
						  (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC 3") (lambda ()
						  (interactive)
						  (split-window-right)
						  (other-window 1)))

;; cursor motion in wrapped lines
(define-key evil-normal-state-map "$" 'evil-end-of-visual-line)
(define-key evil-normal-state-map (kbd "SPC 4") 'evil-end-of-visual-line)
(define-key evil-visual-state-map "$" 'evil-end-of-visual-line)
(define-key evil-visual-state-map (kbd "SPC 4") 'evil-end-of-visual-line)

(define-key evil-normal-state-map (kbd "SPC 5") 'evil-jump-item)
(define-key evil-visual-state-map (kbd "SPC 5") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "SPC x") 'smex)

(define-key evil-normal-state-map (kbd "SPC i w") 'toggle-truncate-lines)

(define-key evil-normal-state-map (kbd "SPC i k") 'describe-key)
(define-key evil-normal-state-map (kbd "SPC i f") 'describe-function)
(define-key evil-normal-state-map (kbd "SPC i v") 'describe-variable)

(define-key evil-normal-state-map (kbd "SPC i t") 'load-theme)

(define-key evil-normal-state-map (kbd "SPC i z") 'zone-rainbow)

(evil-global-set-key 'normal (kbd "C-f") (lambda () (interactive)
					   (evil-scroll-page-down 1)
					   (evil-window-middle)))
(evil-global-set-key 'normal (kbd "C-b") (lambda () (interactive)
					   (evil-scroll-page-up 1)
					   (evil-window-middle)))
(evil-global-set-key 'normal (kbd "C-d") (lambda () (interactive)
					   (evil-window-bottom)
					   (recenter)))
(evil-global-set-key 'normal (kbd "C-u") (lambda () (interactive)
					   (evil-window-top)
					   (recenter)))

(evil-global-set-key 'normal (kbd "z m") 'evil-scroll-line-to-center)
(evil-global-set-key 'normal (kbd "z z") (lambda () (interactive)
					   (evil-scroll-line-to-center (line-number-at-pos))
					   (evil-scroll-line-down (/ (window-total-height) 5))))

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
