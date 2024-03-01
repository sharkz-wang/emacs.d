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
;; enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))
;; no blinking cursor
(blink-cursor-mode -1)

;; force cursor not to jump to center when acrossing screen bounaries
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; suppress the annoying warning bombing from async native-comp
;; e.g., "Warning (comp): ld: warning: -undefined dynamic_lookup
;;        may not work with chained fixups Disable showing Disable
;;        logging"
(setq native-comp-async-report-warnings-errors nil)

(require-package 'which-key)
(which-key-mode)

(require-package 'centered-cursor-mode)

;; common handler setting things in order in all major modes
(defun init-all-major-mode-hdlr ()
    ;; enable truncate-lines-mode (no line wrapping) by default
    (--enable-truncate-lines)
    ;; treat underline as part of a word
    (modify-syntax-entry ?_ "w")
    (centered-cursor-mode 1)
    ;; make cursor stay at center even at tail boundary
    (setq ccm-recenter-at-end-of-file t)
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

(define-key evil-normal-state-map
		(kbd "SPC t c") 'global-centered-cursor-mode)

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
  "u"   'universal-argument
  "SPC" 'helm-M-x
  "x"   'helm-M-x
  "TAB" 'switch-to-last-buffer
  "`"   'switch-to-buffer-before-last-buffer
  "qq"  'save-buffers-kill-terminal
  )
(define-key evil-normal-state-map (kbd "SPC TAB") 'switch-to-last-buffer)

(require 'init-files)

(define-key evil-normal-state-map (kbd "SPC b d")
	    (lambda () (interactive)
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
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w|" '--toggle-follow-mode-view
  "w\\" '--toggle-follow-mode-view
  ;; TODO: fix incorrect font in new frames
  "wn" 'make-frame
)
(define-key evil-normal-state-map (kbd "SPC w q") 'quit-window)

;; vim's $/% alternatives to reduce pressure on your pinky fingers
(define-key evil-normal-state-map   (kbd "SPC 4") 'evil-end-of-line)
(define-key evil-visual-state-map   (kbd "SPC 4") 'evil-end-of-line)
(define-key evil-operator-state-map (kbd "SPC 4") 'evil-end-of-line)

(define-key evil-normal-state-map   (kbd "SPC 5") 'evil-jump-item)
(define-key evil-visual-state-map   (kbd "SPC 5") 'evil-jump-item)
(define-key evil-operator-state-map (kbd "SPC 5") 'evil-jump-item)

;; making it easier to eye-tracking page-up/page-down
(advice-add #'evil-scroll-up
	    :after (lambda (COUNT) (recenter)))
(advice-add #'evil-scroll-down
	    :after (lambda (COUNT) (recenter)))
(advice-add #'evil-scroll-page-up
	    :after (lambda (COUNT) (evil-window-middle)))
(advice-add #'evil-scroll-page-down
	    :after (lambda (COUNT) (evil-window-middle)))

;; make pressing C-c C-c more pinky-friendly
(evil-global-set-key 'normal (kbd "SPC , ,") (kbd "C-c C-c"))

(evil-leader/set-key
  "nf" 'narrow-to-defun
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nw" 'widen
  )

;; cursor motion in wrapped lines

(define-key evil-normal-state-map (kbd "SPC t L") 'toggle-truncate-lines)
(define-key evil-normal-state-map (kbd "SPC i t") 'load-theme)

(define-key evil-normal-state-map (kbd "SPC i z") 'zone-rainbow)

(evil-global-set-key 'normal (kbd "z m") 'evil-scroll-line-to-center)
(evil-global-set-key 'normal (kbd "z z") (lambda () (interactive)
					   (evil-scroll-line-to-center (line-number-at-pos))
					   (evil-scroll-line-down (/ (window-total-height) 5))))

(define-key evil-normal-state-map (kbd "B") (lambda () (interactive)
					      (message (substring
							(shell-command-to-string
							 "git rev-parse --abbrev-ref HEAD")
							0
							-1))))

(require 'init-navigation)

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

(defun switch-to-message-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(evil-global-set-key 'normal (kbd "SPC b M") 'switch-to-message-buffer)

(provide 'init-basics)
