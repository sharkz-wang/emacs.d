(require-package 'evil)
(evil-mode t)

;; bind C-w back in evil emacs state
(define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

;; both key-bindings "'" and "`" set to `evil-goto-mark' for convenience
(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark)

;; bind "j" and "k" for visual line
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)

;; set starting state to normal state
(setq evil-emacs-state-modes '(ediff-mode))
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

(require-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "\\")

(require-package 'evil-nerd-commenter)

(defun evilnc-invert-comment-line-by-line (&optional NUM) (interactive "p")
  (setq evilnc-invert-comment-line-by-line t)
  (evilnc-comment-or-uncomment-lines NUM)
  (setq evilnc-invert-comment-line-by-line nil))

(defun comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
	  (setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-region beg end)))

(defun uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
	  (setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(uncomment-region beg end)))

(defun current-line-empty-p ()
  (save-excursion
	(beginning-of-line)
	(looking-at "[[:space:]]*$")))

(evil-leader/set-key
  "cc" 'evilnc-invert-comment-line-by-line
  "cc" 'comment-region-or-line
  "cu" 'uncomment-region-or-line
  "ci" 'evilnc-invert-comment-line-by-line
  "cy" 'evilnc-copy-and-comment-lines
  "cf" '(lambda()
		  (interactive)
		  (srecode-document-insert-comment)
		  (evil-insert-state)
		  (previous-line)
		  (previous-line)
		  (end-of-line)
		  (if (current-line-empty-p)
			nil
			(newline-and-indent))
		  (next-line)
		  (next-line)
		  (end-of-line)
		  )
  "\\" 'evilnc-comment-operator)

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

(setq evil-global-markers-alist '())

(defun evil-set-marker-local-global (char &optional pos advance)
  (interactive (list (read-char)))
  (evil-set-marker char)
  (evil-add-to-alist
   'evil-global-markers-alist
   char
   (list (buffer-name) (point) (set-marker (make-marker) (point))))
  )

(defun evil-goto-global-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  (interactive (list (read-char)))
  (let (
	(mark (cdr-safe (assq char (default-value
				     'evil-global-markers-alist))))
	)
    (switch-to-buffer (car mark))
    (goto-char (nth 2 mark))
    )
  )

(evil-global-set-key 'normal "m" 'evil-set-marker-local-global)
(evil-global-set-key 'normal "'" 'evil-goto-global-mark-line)

(provide 'init-evil)
