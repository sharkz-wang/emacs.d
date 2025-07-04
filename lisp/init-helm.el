(require-package 'helm)
(require-package 'helm-ag)

(require 'init-helm-defs)

(helm-mode 1)

(helm-autoresize-mode t)
;; disable annoying default minibuffer texts when
;; your cursor happens to be an url
(setq helm-find-files-ignore-thing-at-point t)
;; stop helm from photobombing whole frame
;; when fired in splitted screen
(add-to-list 'display-buffer-alist
             '("\\*helm" (display-buffer-at-bottom))
	     t)
(setq helm-display-function #'display-buffer)

;; hide noise results
(setq helm-ff-skip-boring-files t)

(custom-set-variables
 '(helm-autoresize-max-height 40)
 '(helm-autoresize-min-height 20))

(setq orig-helm-max-height helm-autoresize-max-height)
(setq orig-helm-min-height helm-autoresize-min-height)
(setq curr-helm-max-height helm-autoresize-max-height)
(setq curr-helm-min-height helm-autoresize-min-height)

(add-hook 'helm-major-mode-hook
	  (lambda ()
	    (setq-local sublimity-attractive-centering-width 130)
	   ))

(evil-leader/set-key
  "bb" 'helm-buffers-list
  "rl" 'helm-resume
  "rL" '--helm-resume-select
  "ry" '--helm-show-kill-ring-short
  "rY" '--helm-show-kill-ring-long
  "rP" '--helm-show-kill-ring-short
  "ru" 'undo-tree-visualize
)

(evil-global-set-key 'normal (kbd "SPC RET") 'projectile-switch-to-buffer)

;; XXX: in `helm-do-ag', simply using `let' won't work, as minibuffer would still be
;;      updated after `helm-refresh'
(add-hook 'helm-quit-hook
	  (lambda ()
	    (setq helm-autoresize-max-height curr-helm-max-height)
	    (setq helm-autoresize-min-height curr-helm-min-height)))

(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-u") 'kill-whole-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)

(define-key helm-map (kbd "M-N") 'helm-goto-line-ow-forward)
(define-key helm-map (kbd "M-P") 'helm-goto-line-ow-backward)

(define-key helm-map (kbd "C-c C-c") 'helm-select-action)
(define-key helm-map (kbd "C-c h d k") 'describe-key-and-switch-to-window)

(keymap-unset helm-find-files-map "C-c h")
(keymap-unset helm-find-files-map "C-c /")
(keymap-unset helm-find-files-map "C-c ?")
(keymap-unset helm-find-files-map "C-c r")

(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-l") 'helm-find-files-down-last-level)
(define-key helm-find-files-map (kbd "M-J") 'helm-toggle-visible-mark-forward)
(define-key helm-find-files-map (kbd "M-K") 'helm-toggle-visible-mark-backward)
;; M-backspace remap to up-one-level
(define-key helm-find-files-map (kbd "M-DEL") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-c p") 'evil-paste-from-register)

(define-key helm-find-files-map (kbd "C-c r p") '--helm-show-kill-ring-short)
;; FIXME: I don't use `helm-ff-run-fd' as it does not support multiple
;;        arguments
(define-key helm-find-files-map (kbd "C-c ?") 'helm-ff-run-find-sh-command)

(add-to-list
 'helm-find-files-actions
 (cons "Open magit here" '--helm-projectile-find-file-here) t)
(define-key helm-find-files-map (kbd "C-c /") '--do-helm-projectile-find-file-here)

(define-key helm-find-files-map (kbd "C-c s") 'helm-ff-run-grep-ag)

(define-key helm-map (kbd "C-c C-m") 'helm-toggle-resize-buffer-to-max)
(define-key helm-map (kbd "C-c C-s") 'helm-save-search-session)

(add-to-list
 'helm-find-files-actions (cons "Open magit here" '--helm-open-magit-here)
 t)
(define-key helm-find-files-map (kbd "C-c g") '--do-helm-open-magit-here)

(let ((kmap (alist-get 'keymap helm-source-projectile-files-list))
      (action-list (alist-get 'action helm-source-projectile-files-list)))
  ;; XXX: value of 'action in alist `helm-source-projectile-files-list'
  ;;      was bound upon init, need to dig it out, append, and insert
  ;;      back to alist
  (setf (alist-get 'action helm-source-projectile-files-list)
	(add-to-list
	 'action-list
	 (cons "Open magit here" '--helm-open-magit-here) t))
  (define-key kmap (kbd "C-c g") '--do-helm-open-magit-here))

(let ((kmap (alist-get 'keymap helm-source-projectile-files-list))
      (action-list (alist-get 'action helm-source-projectile-files-list)))
  ;; XXX: value of 'action in alist `helm-source-projectile-files-list'
  ;;      was bound upon init, need to dig it out, append, and insert
  ;;      back to alist
  (setf (alist-get 'action helm-source-projectile-files-list)
	(add-to-list
	 'action-list
	 (cons "Run grep AG action" '--projectile-find-files-ag) t))
  (define-key kmap (kbd "C-c s") 'helm-projectile-ff-run-grep-ag))

(evil-define-key 'normal helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump)
(evil-define-key 'normal helm-ag-mode-map (kbd "q") 'quit-window)

(evil-add-command-properties #'helm-ag-mode-jump :jump t)
(evil-add-command-properties #'helm-maybe-exit-minibuffer :jump t)
(evil-add-command-properties #'helm-ff-RET :jump t)
(evil-add-command-properties #'magit-diff-visit-file :jump t)

(add-hook 'helm-ag-mode-hook
	  (lambda ()
	    (interactive)
	    (read-only-mode -1)
	   ))

(eval-after-load 'helm-ag
  '(progn

     (add-to-list
      'helm-ag--actions (cons "Yank line" '--do-helm-ag-copy-line)
      t)

     (define-key helm-ag-map (kbd "C-n") 'helm-ag--next-file)
     (define-key helm-ag-map (kbd "C-p") 'helm-ag--previous-file)
     (define-key helm-ag-map (kbd "C-c b") #'helm-ag--run-save-buffer)
     (define-key helm-do-ag-map (kbd "C-c b") #'helm-ag--run-save-buffer)
     (define-key helm-ag-map (kbd "C-c e") #'helm-ag-edit)
     (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)

     (define-key helm-ag-map (kbd "C-c y") '--helm-ag-copy-line)
     (define-key helm-ag-map (kbd "C-c p") 'evil-paste-from-register)
     ))


(define-key helm-occur-map (kbd "C-c y") '--helm-occur-copy-line)
(add-to-list
      'helm-occur-actions (cons "Yank line" '--do-helm-occur-copy-line)
      t)

(add-to-list
 'helm-type-buffer-actions (cons "Open magit here" '--helm-type-buffer-open-magit-here)
 t)
(define-key helm-buffer-map (kbd "C-c g") '--do-helm-type-buffer-open-magit-here)

(provide 'init-helm)
