;; skip welcome screen
(setq inhibit-splash-screen t)
;; display column numbers in mode-line
(column-number-mode 1)
(set-default-font "Monaco-15")
;; no tool bars
(tool-bar-mode -1)
;; no memu bar
(menu-bar-mode -1)
;; no scroll bars
(scroll-bar-mode -1)
;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

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

(defun beginning-of-indentation-or-line ()
  "Move point to the beginning of text on the current line; if that is already
   the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-indentation-or-line)

(require-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "bB" 'ibuffer
  )

(evil-leader/set-key
  "ee" 'eval-last-sexp
  )

(evil-leader/set-key
  "u" 'universal-argument
  "SPC" 'smex
  "x" 'smex
  "TAB" 'switch-to-last-buffer
  "qq" 'save-buffers-kill-terminal
  )

(evil-leader/set-key
  "fs" 'save-buffer
  "ff" 'helm-find-files
  )

(evil-leader/set-key
  "bd" (lambda () (interactive) (kill-buffer (current-buffer)))
  )

(evil-leader/set-key
  "ww" 'other-window
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w/" (lambda ()
	 (interactive)
	 (split-window-below)
	 (other-window 1))
  "w-" (lambda ()
	 (interactive)
	 (split-window-right)
	 (other-window 1))
  )

(evil-leader/set-key
  "4" 'evil-end-of-visual-line
  "5" 'evil-jump-item
  )

(evil-leader/set-key
  "hi" 'info
  "hdk" 'describe-key
  "hdf" 'describe-function
  "hdv" 'describe-variable
  )

;; cursor motion in wrapped lines

(define-key evil-normal-state-map (kbd "SPC i w") 'toggle-truncate-lines)
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

(define-key evil-normal-state-map (kbd "_") '(lambda () (interactive)
					       (message (buffer-file-name
							 (window-buffer (minibuffer-selected-window))))))

(define-key evil-normal-state-map (kbd "B") '(lambda () (interactive)
					       (message (substring
							 (shell-command-to-string
							  "git rev-parse --abbrev-ref HEAD")
							 0
							 -1))))
;; dragging line(s) of codes
; TODO: no undo trace
(define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive)
			      (evil-delete-line (- (line-beginning-position) 1) (line-end-position) t)
			      (evil-previous-line)
			      (evil-end-of-line)
			      (evil-paste-after 1)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-next-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-previous-line)
			      (beginning-of-line-text)
			      ))

(define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive)
			      (evil-delete-line (- (line-beginning-position) 1) (line-end-position) t)
			      (evil-next-line)
			      (evil-end-of-line)
			      (evil-paste-after 1)
			      (evil-previous-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-next-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (beginning-of-line-text)
			      ))

; TODO: review low-quality code
(define-key evil-visual-state-map (kbd "M-k") (lambda () (interactive)
						(let (
						      (region-line-num (count-lines (region-beginning) (region-end)))
						      )
						  (evil-delete-line (region-beginning) (- (region-end) 1) t)
						  (evil-previous-line 2)
						  (evil-paste-after 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  (evil-indent-line (region-beginning) (region-end))
						  (evil-next-line (- region-line-num 1))
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-next-line 1)
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-previous-line 1)
						  (evil-visual-line)
						  )
						))

(define-key evil-visual-state-map (kbd "M-j") (lambda () (interactive)
						(let (
						      (region-line-num (count-lines (region-beginning) (region-end)))
						      )
						  (evil-delete-line (region-beginning) (- (region-end) 1) t)
						  (evil-paste-after 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  (evil-indent-line (region-beginning) (region-end))
						  (evil-next-line (- region-line-num 1))
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-previous-line region-line-num)
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-next-line 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  )
						))

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
;; end settings for smex

(provide 'init-basics)
