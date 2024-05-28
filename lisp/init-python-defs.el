(require 'init-insert-snippets-defs)

(defun python-insert-reminder-comment (keyword) (interactive)
       (if (current-line-empty-p)
	   (progn
	     (end-of-line)
	     (insert (format "# %s: " keyword))
	     (indent-according-to-mode)
	     (evil-insert-state)
	     )
	 (progn
	   (beginning-of-line)
	   (newline)
	   (previous-line)
	   (end-of-line)
	   (insert (format "# %s: " keyword))
	   (indent-according-to-mode)
	   (evil-insert-state)
	   )
	 ))

(defun python-insert-for-loop ()
  (interactive)
    (expand-yasnippet-from-keyword "for ... in ... : ..."))

(defun python-insert-print ()
  (interactive)
    (expand-yasnippet-from-keyword "print"))

(defun python-insert-debug ()
  (interactive)
    (expand-yasnippet-from-keyword "debug"))

(defun python-insert-ptyhon-todo-comment ()
  (interactive) (python-insert-reminder-comment "TODO"))

(defun python-insert-ptyhon-fixme-comment ()
  (interactive) (python-insert-reminder-comment "FIXME"))

(defun python-insert-ptyhon-xxx-comment ()
  (interactive) (python-insert-reminder-comment "XXX"))

(defun python-insert-new-arg () (interactive)
       (search-forward ")")
       (left-char)
       (insert ", ")
       (evil-insert-state))

(defun python-avy-insert-new-arg () (interactive)
       (avy-goto-char-in-line ?,)
       (right-char)
       (insert " ,")
       (left-char)
       (evil-insert-state))

(defun python-reverse-truth-value () (interactive)
       (if (string= (thing-at-point 'word) "True")
	   (progn
	     (evil-backward-word-end 1)
	     (evil-forward-word-end 1)
	     (evil-delete-backward-word)
	     (delete-forward-char 1)
	     (insert "False")
	     )
	 (if (string= (thing-at-point 'word) "False")
	     (progn
	       (evil-backward-word-end 1)
	       (evil-forward-word-end 1)
	       (evil-delete-backward-word)
	       (delete-forward-char 1)
	       (insert "True")
	       )
	   )
	 ))

(provide 'init-python-defs)
