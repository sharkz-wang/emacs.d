;; place experimental code here

(require 'init-vcs)

(evil-define-minor-mode-key 'normal
    'ediff-mode-map "j" 'evil-collection-ediff-scroll-down-1)
(evil-define-minor-mode-key 'normal
    'ediff-mode-map "k" 'evil-collection-ediff-scroll-up-1)

(evil-define-minor-mode-key 'normal
    'ediff-mode-map (kbd "C-f") 'evil-collection-ediff-scroll-down)
(evil-define-minor-mode-key 'normal
    'ediff-mode-map (kbd "C-b") 'evil-collection-ediff-scroll-up)

(defun init-ediff-buffer ()
  (when (display-graphic-p)
    (progn
      ;; TODO: make it into single line
      (zoom-frm-out)
      (zoom-frm-out)
      (zoom-frm-out)
      (zoom-frm-out)
      (zoom-frm-out)
      (zoom-frm-out)
      )
    )

  ;; disable tabs, which makes screen crowded
  (centaur-tabs-mode -1)
  ;; FIXME: hacky trick to refresh and minimize control panel
  (ediff-toggle-help)
  (ediff-toggle-help)
)
(add-hook 'ediff-startup-hook 'init-ediff-buffer)

(defun quit-ediff-buffer ()
  (when (display-graphic-p)
    (progn
      (zoom-frm-in)
      (zoom-frm-in)
      (zoom-frm-in)
      (zoom-frm-in)
      (zoom-frm-in)
      (zoom-frm-in)
      )
    )

  ;; TODO: enable it selectively
  ;; restore tabs
  (centaur-tabs-mode 1)
)
(add-hook 'magit-ediff-quit-hook 'quit-ediff-buffer)
(add-hook 'ediff-quit-hook 'quit-ediff-buffer)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))

(require-package 'gumshoe)
;; package gumshoe's dependency
(require-package 'consult)

(setq gumshoe-ignored-major-modes
      '(emacs-lisp-mode          ;; elisp buffers
        org-mode                 ;; org-mode buffers
        messages-buffer-mode     ;; *Messages*
        help-mode                ;; *Help*
        ;; XXX: don't include *scratch*, as there should always
        ;;      be at least one instance returned or `consult-global-mark'
        ;;      will list all of them anyway.
        ;;      scratch buffer was the best for this job.
        ;; lisp-interaction-mode ;; *scratch*
        ))

(defun --get-marker-buf-major-mode (marker)
    (with-current-buffer (marker-buffer marker) major-mode))

(defun --gumshoe-is-major-mode-ignored (maj-mode)
    (-any? #'identity
           (mapcar (lambda (maj) (equalp maj-mode maj))
                   gumshoe-ignored-major-modes))
)

(defun --get-marker-list-ignored-removed ()
    (seq-filter (lambda (marker)
                  (not (--gumshoe-is-major-mode-ignored
                           (--get-marker-buf-major-mode marker))))
                (ring-elements (oref gumshoe--global-backlog log)))
)

(global-gumshoe-mode 1)
(defun consult-gumshoe-global ()
    (interactive)
    (consult-global-mark (--get-marker-list-ignored-removed))
)


(evil-leader/set-key "rh" 'consult-gumshoe-global)

(evil-define-text-object evil-avy-word (count &optional beg end type)
    "Select a word, with avy."
    (save-excursion
      (evil-avy-goto-char)
      (evil-a-word count beg end type)
    )
)

(define-key evil-outer-text-objects-map "j" 'evil-avy-word)
(define-key evil-inner-text-objects-map "j" 'evil-avy-word)

;; make magit diff buffers started with ...
;;     1. cursor moved to the center line
;;     2. buffer scrolls around the cursor
;; so we don't have to wait that little jerk crawling
;; to the last line when reading patch diffs
(defun --setup-magit ()
    (centered-cursor-mode 1)
)
(defun --advice-magit-diff-setup-buffer (RANGE TYPEARG ARGS FILES &optional LOCKED)
    ;; this statement is too early in `magit-diff-mode-hook'
    (move-to-window-line nil)
)

(advice-add 'magit-diff-setup-buffer :after #'--advice-magit-diff-setup-buffer)
(add-hook 'magit-diff-mode-hook '--setup-magit)

;; number of characters until we place a indicator bar
(setq display-fill-column-indicator-column 80)
;; character we used to resemble a bar
(setq display-fill-column-indicator-character ?│)
;; face of the indicator bar
(custom-set-faces '(fill-column-indicator
		    ((t (:foreground "#2D2D2D")))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (display-fill-column-indicator-mode 1)))
(add-hook 'python-mode-hook
	  (lambda () (display-fill-column-indicator-mode 1)))
(add-hook 'c-mode-hook
	  (lambda () (display-fill-column-indicator-mode 1)))
(add-hook 'c++-mode-hook
	  (lambda () (display-fill-column-indicator-mode 1)))
(add-hook 'magit-diff-mode-hook
	  (lambda () (display-fill-column-indicator-mode 1)))

(defun --setup-git-commit-msg-column-indicator ()
    (display-fill-column-indicator-mode 1)
    ;; it's recommended to have < 50 character in commit subject
    ;; so let's place a bar at here
    (setq-local display-fill-column-indicator-column 50)
    ;; we use a lot of apostroph "'" in commit messages, which happen
    ;; to be my favorite company leader key... let's turn if off!
    (company-mode -1)
)

(add-hook 'git-commit-setup-hook
	  '--setup-git-commit-msg-column-indicator)

(require-package 'evil-lion)
(evil-lion-mode 1)

;; run external program asynchronously
;; e.g., (terminal-run-command-async "ls")
;;       (terminal-run-command-async "vim" (buffer-file-name))
(defun osx-terminal-run-command-async (&rest args)
  (apply 'start-process
	 (append
	  (quote "iTerm2" "*iTerm2*"
		 (expand-file-name "script/osx_run.sh"
				   user-emacs-directory))
	  args))
)

(load "hierarchy.el")
(require-package 'call-graph)
(define-key evil-normal-state-map (kbd "SPC g c") 'call-graph)

(provide 'staging)
