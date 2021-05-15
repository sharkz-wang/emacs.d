
(defun emacs-lisp-insert-reminder-comment (keyword) (interactive)
       (if (current-line-empty-p)
	   (progn
	     (end-of-line)
	     (insert (format ";; %s: " keyword))
	     (indent-according-to-mode)
	     (evil-insert-state)
	     )
	 (progn
	   (beginning-of-line)
	   (newline)
	   (previous-line)
	   (end-of-line)
	   (insert (format ";; %s: " keyword))
	   (indent-according-to-mode)
	   (evil-insert-state)
	   )
	 ))

(defun emacs-lisp-insert-for-loop ()
  (interactive)
  (expand-yasnippet-from-keyword "mapc"))
(defun emacs-lisp-insert-print ()
  (interactive)
  (expand-yasnippet-from-keyword "message"))

(defun emacs-lisp-insert-todo-comment ()
  (interactive)
  (emacs-lisp-insert-reminder-comment "TODO"))
(defun emacs-lisp-insert-fixme-comment ()
  (interactive)
  (emacs-lisp-insert-reminder-comment "FIXME"))
(defun emacs-lisp-insert-xxx-comment ()
  (interactive)
  (emacs-lisp-insert-reminder-comment "XXX"))

;; (defun emacs-lisp-insert-new-arg ()
;;   (interactive)
;;   )
;; (defun emacs-lisp-avy-insert-new-arg ()
;;   (interactive)
;;   )

(provide 'init-elisp)
