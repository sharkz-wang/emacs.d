;; place experimental code here

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
           (mapcar (lambda (maj) (cl-equalp maj-mode maj))
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

(defun y-or-y-you-dont-have-to-type-p (func &rest args)
  (if (and (eq major-mode 'ediff-mode)
	   (or (string-equal (nth 0 args) "You’ve previously copied diff region 1 to buffer C.  Confirm? ")
	    (string-equal (nth 0 args) "Quit this Ediff session? ")))
      t (apply func args)))
(advice-add 'y-or-n-p :around #'y-or-y-you-dont-have-to-type-p)

(evil-global-set-key 'normal (kbd "SPC w [") 'evil-window-top-left)
(evil-global-set-key 'normal (kbd "SPC w ]") 'evil-window-bottom-right)

(evil-global-set-key 'normal (kbd "SPC w h") 'evil-window-left)
(evil-global-set-key 'normal (kbd "SPC w l") 'evil-window-right)
(evil-global-set-key 'normal (kbd "SPC w j") 'evil-window-down)
(evil-global-set-key 'normal (kbd "SPC w k") 'evil-window-up)

(evil-global-set-key 'normal (kbd "SPC w =") 'balance-windows)

(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w [") 'evil-window-top-left)
(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w ]") 'evil-window-bottom-right)

(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w h") 'evil-window-left)
(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w l") 'evil-window-right)
(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w j") 'evil-window-down)
(evil-define-minor-mode-key 'normal 'ediff-mode-map (kbd "SPC w k") 'evil-window-up)

(evil-define-minor-mode-key 'normal 'magit-mode-map (kbd "SPC w h") 'evil-window-left)
(evil-define-minor-mode-key 'normal 'magit-mode-map (kbd "SPC w l") 'evil-window-right)
(evil-define-minor-mode-key 'normal 'magit-mode-map (kbd "SPC w j") 'evil-window-down)
(evil-define-minor-mode-key 'normal 'magit-mode-map (kbd "SPC w k") 'evil-window-up)

(evil-define-minor-mode-key 'normal 'magit-mode-map (kbd "SPC w =") 'balance-windows)

(defun auto-save-bookmarks (&rest r)
  (bookmark-save))
(advice-add 'bookmark-set :after #'auto-save-bookmarks)

;; TODO: move it to init-teleport.el
(define-key teleport-map "d"
	    (lambda () (interactive) (teleport-do-action "~/documents")))

(custom-set-faces
 '(magit-diff-revision-summary-highlight ((t :foreground "#FFFFFF"))))
(custom-set-faces
 '(magit-diff-revision-summary ((t :foreground "#FFFFFF"))))


(defun --display-staged-file-history ()
  (interactive)
  (split-window-right)
  (magit-log-head (append '("--decorate" "-n20" "--")
			  (magit-staged-files)))
  (other-window 1))

;; display auxilary commit history for staged files
(advice-add 'magit-commit-diff :after '--display-staged-file-history)

(defun --postprocess-doom-modeline-buffer-name (s)
  (cond
   ((string-prefix-p (expand-file-name "~") (buffer-file-name))
    (concat "home:  " s))
   (t s)))

(advice-add 'doom-modeline--buffer-file-name
	    :filter-return '--postprocess-doom-modeline-buffer-name)

;; XXX: dirty way to force `helm-do-ag-buffers' to be limited to
;;      opened buffers in current project
(defun helm-ag--file-visited-buffers ()
  "Not documented."
  (let ((bufs (cl-loop for buf in
		       (mapcar (lambda (bname) (get-buffer bname))
			       (projectile-project-buffer-names))
		       when (buffer-file-name buf)
		       collect it)))
    (if (not helm-ag-ignore-buffer-patterns)
	bufs
      (cl-loop for buf in bufs
	       when (helm-ag--search-buffer-p buf)
	       collect buf))))

(defun --restore-pos-after-evil-cmd (func &rest r)
  (let ((prev-pos (point)))
    (apply func r) (goto-char prev-pos))
  )

(advice-add 'evil-yank :around '--restore-pos-after-evil-cmd)
(advice-add 'evil-indent :around '--restore-pos-after-evil-cmd)

;; TODO: FIXME: ugly global variable
(setq visual-start-pos nil)

(advice-add 'evil-visual-char :before
	    #'(lambda (&rest args) (interactive) (setq visual-start-pos (point))))
(advice-add 'evil-visual-line :before
	    #'(lambda (&rest args) (interactive) (setq visual-start-pos (point))))
(add-hook 'evil-visual-state-exit-hook
	  #'(lambda () (interactive) (goto-char visual-start-pos)))

;; TODO: FIXME: ugly global variable
(setq restore-cursor-pos nil)

(defun --restore-cursor-settings ()
  (interactive)
  ;; TODO: FIXME: not considering original mode state
  (global-centered-cursor-mode 1)
  (goto-char restore-cursor-pos)
  (set-face-attribute 'hl-line nil :background "#000000")
  (pulse-momentary-highlight-one-line (point))
  (define-key evil-normal-state-map "q" 'evil-record-macro)
  )

(defun --start-virtual-cursor ()
  (interactive)
  (setq restore-cursor-pos (point))
  (evil-avy-goto-char)
  (global-centered-cursor-mode 0)
  (pulse-momentary-highlight-one-line (point))
  (set-face-attribute 'hl-line nil :background "#4D4C4D")
  (define-key evil-normal-state-map "q" '--restore-cursor-settings)
  )

(define-key evil-normal-state-map (kbd "SPC j") '--start-virtual-cursor)

(set-face-attribute 'region nil :background monokai-gray)
(set-face-attribute 'show-paren-match nil
		    :background monokai-orange :foreground monokai-foreground)

(advice-add 'make-frame :after
	#'(lambda (&rest args) (interactive)
	    (pulse-momentary-highlight-one-line (point))))

(provide 'staging)
