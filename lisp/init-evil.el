(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(require-package 'evil)

(require-package 'evil-collection)
;; this breaks company-mode, let's disable it
(setq evil-collection-company-use-tng nil)
(evil-collection-init)

(evil-mode t)

;; made `diw' not to delete newlines
(evil-define-text-object evil-inner-word (count &optional beg end type)
  "Select inner word."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?\n "w")
    (evil-select-inner-object 'evil-word beg end type count))
  )

(require-package 'undo-tree)
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

(setq evil-global-markers-alist '())

(defun clear-evil-global-markers-alist ()
  (interactive)
  (setq evil-global-markers-alist '())
  (message "All global markers deleted"))
(evil-global-set-key 'normal (kbd "SPC m c") 'clear-evil-global-markers-alist)

(defun evil-set-marker-local-global (char &optional pos advance)
  (interactive (list (read-char)))
  (evil-set-marker char)
  (evil-add-to-alist
   'evil-global-markers-alist
   char
   (if (buffer-file-name)
       (list 'file (buffer-file-name) (point))
     (if (eq major-mode 'nov-mode)
	 (progn
	   (nov-save-place char nov-documents-index (point))
	   (list 'epub (buffer-name) 0))
       (list 'buffer (buffer-name) (point))))))

(defun get-global-mark (char)
  (cdr-safe
   (assq char
	 (default-value
	   'evil-global-markers-alist))))

(defun evil-goto-global-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  (interactive (list (read-char)))
  (let ((mark (get-global-mark char)))
    (if (not (equalp mark nil))
	(let ((type (car mark))
	      (name (nth 1 mark))
	      (pos (nth 2 mark)))
	  (progn
	    (if (equalp (car mark) 'file)
		(progn
		  (find-file name)
		  (goto-char pos))
	      (if (equalp (car mark) 'epub)
		  (progn
		    (let ((nov-documents-index (cdr (car (nov-saved-place char))))
			  (pos (cdr (car (cdr (nov-saved-place char))))))
		      (message name)
		      (switch-to-buffer name)
		      (nov-render-document)
		      (goto-char pos)))
		(progn
		  (if (get-buffer name)
		      (progn
			(switch-to-buffer name)
			(goto-char pos))
		    (message
		     (format "Buffer `%s' does not exist" (nth 1 mark)))
		    ))))))
      (progn (message (format "Mark `%c' undefined" char))))))

(evil-global-set-key 'normal "m" 'evil-set-marker-local-global)
(evil-global-set-key 'normal "'" 'evil-goto-global-mark-line)

(evil-global-set-key 'normal (kbd "C-M-o") 'evil-jump-backward)
(evil-global-set-key 'normal (kbd "C-M-i") 'evil-jump-forward)

(defun curr-line-remove-trailing-whitespace ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(provide 'init-evil)
