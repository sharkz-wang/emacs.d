(require 'init-basics-defs)

;; skip welcome screen
(setq inhibit-splash-screen t)
;; display column numbers in mode-line
(column-number-mode 1)
;; no tool bars
(tool-bar-mode -1)
;; no memu bar
(menu-bar-mode -1)
;; no scroll bars
(when (not (equal window-system nil))
    (scroll-bar-mode -1)
 )
;; no blinking cursor
(blink-cursor-mode -1)
;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; suppress the annoying warning bombing from async native-comp
;; e.g., "Warning (comp): ld: warning: -undefined dynamic_lookup
;;        may not work with chained fixups Disable showing Disable
;;        logging"
(setq native-comp-async-report-warnings-errors nil)

;; common handler setting things in order in all major modes
(defun init-all-major-mode-hdlr ()
    ;; treat underline as part of a word
    (modify-syntax-entry ?_ "w")
)
(add-hook 'after-change-major-mode-hook 'init-all-major-mode-hdlr)

(setq custom-file "~/.emacs.d/.cust-vars.el")
;; this auto-generated custom-file things always gets annoying
;; when debugging lisp codes, let just disable it completely
;; ;; (when (not (file-exists-p custom-file))
;; ;;   (with-temp-buffer (write-file custom-file))
;; ;;   )
;; ;; (load custom-file)

(setq recentf-max-saved-items 200)

(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 3
      version-control t)
(setq backup-directory-alist (list (cons "."  (concat user-emacs-directory "backups"))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; always use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

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
  "bi" 'ibuffer
  "bw" 'read-only-mode
  "br" 'rename-buffer
  "bs" (lambda () (interactive) (switch-to-buffer "*scratch*"))
  "bq" 'bury-buffer
  )

(evil-leader/set-key
  "ee" 'eval-last-sexp
  "eb" 'eval-buffer
  "er" 'eval-region
  )

(defun switch-to-buffer-before-last-buffer ()
  (interactive)
  (switch-to-buffer
   (nth 2 (seq-filter
	   (lambda (buf)
	     (not (string-match-p ".*?\*Minibuf.*?\*.*?" (buffer-name buf))))
	   (buffer-list)))))

(evil-leader/set-key
  "u" 'universal-argument
  "SPC" 'smex
  "x" 'smex
  "TAB" 'switch-to-last-buffer
  "`" 'switch-to-buffer-before-last-buffer
  "qq" 'save-buffers-kill-terminal
  )
(define-key evil-normal-state-map (kbd "SPC TAB") 'switch-to-last-buffer)

(require 'init-files)

(define-key evil-normal-state-map (kbd "SPC b d")
  '(lambda ()
     (interactive)
     (kill-buffer (current-buffer))
     (if (> (length (window-list)) 1)
	 (delete-window))))

;; make split-window functions move cursor to the new window for you
(advice-add 'split-window-right
	    :after (lambda (&optional SIZE) (interactive) (other-window 1)))
(advice-add 'split-window-below
	    :after (lambda (&optional SIZE) (interactive) (other-window 1)))

(evil-global-set-key 'normal (kbd "SPC w /") 'split-window-right)
(evil-global-set-key 'normal (kbd "SPC w -") 'split-window-below)

(evil-leader/set-key
  "ww" 'other-window
  "wj" 'ns-next-frame
  "wk" 'ns-prev-frame
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w|" '--toggle-follow-mode-view
  "w\\" '--toggle-follow-mode-view
  ;; TODO: fix incorrect font in new frames
  "wn" 'make-frame
)
(define-key evil-normal-state-map (kbd "SPC w q") 'quit-window)

(evil-leader/set-key
  "4" 'evil-end-of-line
  "5" 'evil-jump-item
  )

(evil-leader/set-key
  "nf" 'narrow-to-defun
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nw" 'widen
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

(global-set-key (kbd "M-x") 'helm-M-x)
;; end settings for smex

(evil-define-key 'normal Info-mode-map
  "gg" 'evil-goto-first-line
  "d" 'Info-directory
  "i" 'Info-goto-node
  "q" 'Info-exit
  )

;; (with-current-buffer "*Messages*"
  ;; (evil-normalize-keymaps)
  ;; (evil-leader-mode 1)
  ;; (evil-local-set-key 'normal "q" 'quit-window)
  ;; )

(evil-define-key 'normal help-mode-map
  "q" 'quit-window
  )

(evil-define-key 'normal debugger-mode-map
  "q" 'quit-window
  )

;; TODO: move to init-edit
(require-package 'multiple-cursors)

(evil-leader/set-key
  ",i" 'mc/edit-lines
  )

(defun browser-google-search (query)
  (interactive)
  (browse-url (format
	       "https://www.google.com/search?q=%s"
	       (url-encode-url query)))
  )

(defun create-extended-window ()
  (interactive)
  (recenter)
  (split-window-horizontally)
  (other-window 1)
  (recenter)
  (scroll-up-line (- (window-total-height) 3))
  (evil-window-middle)
  (other-window 1)
  (scroll-all-mode 1)
 )

(evil-leader/set-key
  "wv" 'create-extended-window
  )

(push 'ibuffer-mode evil-snipe-disabled-modes)
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (interactive)
	    (turn-off-evil-mode)
	    (evil-normal-state)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "SPC s s") 'helm-occur)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "/") 'helm-occur)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "s a") 'ibuffer-do-sort-by-alphabetic)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename/process)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "s m") 'ibuffer-do-sort-by-major-mode)
	    (evil-define-key 'normal ibuffer-mode-map (kbd "s r") 'ibuffer-do-sort-by-recency)
	    ))

(provide 'init-basics)
