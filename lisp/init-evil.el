(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(require-package 'evil)

(require-package 'evil-collection)
;; this breaks company-mode, let's disable it
(setq evil-collection-company-use-tng nil)
(evil-collection-init)

(evil-mode t)

;; don't including newline when hitting $ in visual-mode, which is
;; fucking annoying
(setq evil-v$-excludes-newline t)

;; made `diw' not to delete newlines
(evil-define-text-object evil-inner-word (count &optional beg end type)
  "Select inner word."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?\n "w")
    (evil-select-inner-object 'evil-word beg end type count))
  )

(require-package 'undo-tree)
;; prevent undo-tree temp files (.*.~undo-tree~) from scattering all over the place
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(global-undo-tree-mode 1)
;; bind C-w back in evil emacs state
(define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
(customize-set-variable 'evil-undo-system 'undo-tree)
(customize-set-variable 'evil-want-fine-undo t)

;; both key-bindings "'" and "`" set to `evil-goto-mark' for convenience
(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark)

;; bind "j" and "k" for visual line
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)

;; set starting state to normal state
(setq evil-emacs-state-modes '(ediff-mode))
(setq evil-normal-state-modes '(dired-mode))
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)
(setq evil-default-state 'normal)
(add-hook 'evil-local-mode-hook #'evil-normal-state)
;; end setting starting state

(require-package 'evil-surround)
(global-evil-surround-mode t)

(evil-define-key 'operator evil-surround-mode-map "S" 'evil-surround-edit)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(require-package 'evil-snipe)
;; turn off annoying highlight when matches found
(setq evil-snipe-enable-highlight nil)
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
;; evil-surround shadows my useful 's' keybinding for staging hunks
(add-hook 'magit-mode-hook 'turn-off-evil-surround-mode)

(require-package 'evil-matchit)
(global-evil-matchit-mode t)

(require-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "\\")

(require-package 'evil-nerd-commenter)

(defun current-line-empty-p ()
  (save-excursion
	(beginning-of-line)
	(looking-at "[[:space:]]*$")))

;; set kj and jk as escape keys
(defadvice
  evil-insert-state
  (around emacs-state-instead-of-insert-state activate)
  (evil-emacs-state))

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map "k" #'cofi/maybe-exit-kj)
(define-key evil-emacs-state-map "j" #'cofi/maybe-exit-jk)

(evil-define-command
  cofi/maybe-exit-kj
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
	(ignore-errors (insert "k"))
	(let ((evt (read-event (format "Insert %c to exit insert state" ?j)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?j))
		 (ignore-errors (delete-char -1))
		 (set-buffer-modified-p modified)
		 (push 'escape unread-command-events))
		(t (setq unread-command-events (append unread-command-events
											   (list evt))))))))

(evil-define-command
  cofi/maybe-exit-jk
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
	(ignore-errors (insert "j"))
	(let ((evt (read-event (format "Insert %c to exit insert state" ?k)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?k))
		 (ignore-errors (delete-char -1))
		 (set-buffer-modified-p modified)
		 (push 'escape unread-command-events))
		(t (setq unread-command-events (append unread-command-events
											   (list evt))))))))
;; end setting special escape keys

(define-key evil-normal-state-map (kbd "SPC i 3") (lambda () (interactive)
						    (evil-scroll-line-to-center (line-number-at-pos))
						    (split-window-right)
						    (other-window 1)
						    (evil-scroll-line-to-center (line-number-at-pos))
						    (evil-window-bottom 1)
						    (evil-scroll-line-to-top (line-number-at-pos))
						    (evil-window-middle)
						    (other-window 1)
						    ))
(define-key evil-normal-state-map (kbd "C-M-e") (lambda () (interactive)
						  (other-window 1)
						  (evil-scroll-line-down 1)
						  (other-window 1)
						  (evil-scroll-line-down 1)
						  ))
(define-key evil-normal-state-map (kbd "C-M-y") (lambda () (interactive)
						  (other-window 1)
						  (evil-scroll-line-up 1)
						  (other-window 1)
						  (evil-scroll-line-up 1)
						  ))

;; I don't like the base bahavior where only capital letters
;; get treated as global markers
;; Let's override it
(advice-add 'evil-global-marker-p :override (lambda (char) t))

(setq evil-markers-alist-orig evil-markers-alist)
(defun clear-evil-markers-alist ()
  (interactive)
  (setq-default evil-markers-alist evil-markers-alist-orig)
  (message "All markers deleted"))
(evil-global-set-key 'normal (kbd "SPC m c") 'clear-evil-markers-alist)

(evil-global-set-key 'normal (kbd "C-M-o") 'evil-jump-backward)
(evil-global-set-key 'normal (kbd "C-M-i") 'evil-jump-forward)

;; saving evil marks to save file ~/.emacs.d/.evil-marks and
;; restore them in next session
(require-package 'save-sexp)

;; (defun --save-markers-alist (&rest args)
;;   (let ((buf (find-file-noselect
;; 	      (expand-file-name
;; 	       ".evil-marks" user-emacs-directory))))
;;     ;; XXX: using `save-sexp-save-setq' with file name as first
;;     ;;      arg causes other buffers drop to fundamental-mode.
;;     ;;      root cause is unknown.
;;     (save-sexp-save-setq buf 'evil-markers-alist)
;;     (with-current-buffer buf (save-buffer)))
;; )

;; ;; ;; setting up advice-function/hook
;; ;;;; after any new mark registered
;; (advice-add 'evil-set-marker-local-global :after
;; 	    #'--save-markers-alist)
;; ;;;; after marks cleared
;; (advice-add 'clear-evil-global-markers-alist :after
;; 	    #'--save-markers-alist)
;; ;;;; before exiting emacs
;; (add-hook 'kill-emacs-hook '--save-markers-alist)

;; ;; restore save file of evil marks
;; (load (expand-file-name ".evil-marks" user-emacs-directory))

(defun curr-line-remove-trailing-whitespace ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(provide 'init-evil)
