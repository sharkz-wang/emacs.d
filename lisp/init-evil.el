(require-package 'evil)
(evil-mode t)

;; bind C-w back in evil emacs state
(define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

;; set starting state to normal state
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)
(setq evil-default-state 'normal)
;; end setting starting state

(require-package 'evil-surround)
(global-evil-surround-mode t)

(require-package 'evil-snipe)
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

(require-package 'evil-matchit)
(global-evil-matchit-mode t)

;; set kj, jk, fd, and df as escape keys
(defadvice
  evil-insert-state
  (around emacs-state-instead-of-insert-state activate)
  (evil-emacs-state))

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map "k" #'cofi/maybe-exit-kj)
(define-key evil-emacs-state-map "j" #'cofi/maybe-exit-jk)
(define-key evil-emacs-state-map "f" #'cofi/maybe-exit-fd)
(define-key evil-emacs-state-map "d" #'cofi/maybe-exit-df)

(evil-define-command
  cofi/maybe-exit-kj
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
	(insert "k")
	(let ((evt (read-event (format "Insert %c to exit insert state" ?j)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?j))
		 (delete-char -1)
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
	(insert "j")
	(let ((evt (read-event (format "Insert %c to exit insert state" ?k)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?k))
		 (delete-char -1)
		 (set-buffer-modified-p modified)
		 (push 'escape unread-command-events))
		(t (setq unread-command-events (append unread-command-events
											   (list evt))))))))

(evil-define-command
  cofi/maybe-exit-fd
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
	(insert "f")
	(let ((evt (read-event (format "Insert %c to exit insert state" ?d)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?d))
		 (delete-char -1)
		 (set-buffer-modified-p modified)
		 (push 'escape unread-command-events))
		(t (setq unread-command-events (append unread-command-events
											   (list evt))))))))

(evil-define-command
  cofi/maybe-exit-df
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
	(insert "d")
	(let ((evt (read-event (format "Insert %c to exit insert state" ?f)
						   nil 0.5)))
	  (cond
		((null evt) (message ""))
		((and (integerp evt) (char-equal evt ?f))
		 (delete-char -1)
		 (set-buffer-modified-p modified)
		 (push 'escape unread-command-events))
		(t (setq unread-command-events (append unread-command-events
											   (list evt))))))))
;; end setting special escape keys

(provide 'init-evil)
