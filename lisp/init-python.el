(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(require-package 'elpy)
(elpy-enable)

(setq elpy-modules
      (remove 'elpy-module-highlight-indentation elpy-modules))
(setq elpy-modules
      (remove 'elpy-module-flymake elpy-modules))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(eval-after-load "elpy"
  '(progn
     (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-kill)
     (define-key elpy-mode-map (kbd "C-c C-p") 'run-python)
     (define-key elpy-mode-map (kbd "C-c l")
       (lambda ()
	  (interactive)
	  (switch-to-buffer "*Python*")
	  (let ((comint-buffer-maximum-size 0))
	    (comint-truncate-buffer))
	  (switch-to-last-buffer)
	  ))
     ))

(add-hook
 'python-mode-hook
 (lambda
   ()
   (modify-syntax-entry ?_ "w")
   
   ;; (require 'init-semantic)
   (require 'init-gtags)
   (require 'init-company)

   (require-package 'evil-indent-textobject)

   (define-key evil-normal-state-map (kbd "SPC p 3")
     (lambda
       ()
       (interactive)
       (split-window-right)
       (other-window 1)
       (switch-to-buffer "*Python*")
       (other-window 1)))

   (defun python-insert-print ()
     (interactive)
     (insert "print(\"\")")
     (left-char 2)
     (evil-insert-state)
     (indent-according-to-mode))
   (define-key python-mode-map (kbd "C-c d p") 'python-insert-print)
   (evil-define-key 'motion python-mode-map (kbd "SPC d p") 'python-insert-print)

   (defun python-insert-formated-string ()
     (interactive)
     (insert "\"\" % ()")
     (left-char 6)
     (evil-insert-state)
     (indent-according-to-mode))
   (define-key python-mode-map (kbd "C-c d s") 'python-insert-formated-string)
   (evil-define-key 'motion python-mode-map (kbd "SPC d s") 'python-insert-formated-string)

   (defun python-append-formated-string-param ()
     (interactive)
     (search-forward "\"")
     (insert " % ()")
     (left-char)
     (evil-insert-state)
     (indent-according-to-mode))
   (define-key python-mode-map (kbd "C-c a s") 'python-append-formated-string-param)
   (evil-define-key 'motion python-mode-map (kbd "SPC a s") 'python-append-formated-string-param)

   (defun python-insert-formated-string-print ()
     (interactive)
     (insert "print(\"\" % ())")
     (left-char 7)
     (evil-insert-state)
     (indent-according-to-mode))
   (define-key python-mode-map (kbd "C-c d P") 'python-insert-formated-string-print)
   (evil-define-key 'motion python-mode-map (kbd "SPC d P") 'python-insert-formated-string-print)

   (defun python-insert-exit ()
     (interactive)
     (insert "exit(1)")
     (indent-according-to-mode))
   (define-key python-mode-map (kbd "C-c d e") 'python-insert-exit)
   (evil-define-key 'motion python-mode-map (kbd "SPC d e") 'python-insert-exit)

   (defun python-insert-new-arg () (interactive)
	  (search-forward ")")
	  (left-char)
	  (insert ", ")
	  (evil-insert-state))
   (define-key python-mode-map (kbd "C-c a ,") 'python-insert-new-arg)
   (evil-define-key 'motion python-mode-map (kbd "SPC a ,") 'python-insert-new-arg)

   (defun python-avy-insert-new-arg () (interactive)
	  (avy-goto-char-in-line ?,)
	  (right-char)
	  (insert " ,")
	  (left-char)
	  (evil-insert-state))
   (define-key python-mode-map (kbd "C-c i ,") 'python-avy-insert-new-arg)
   (evil-define-key 'motion python-mode-map (kbd "SPC i ,") 'python-avy-insert-new-arg)

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
   (define-key python-mode-map (kbd "C-c d i") 'python-reverse-truth-value)
   (evil-define-key 'motion python-mode-map (kbd "SPC d i") 'python-reverse-truth-value)

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

   (defun python-insert-ptyhon-todo-comment ()
     (interactive)
     (python-insert-reminder-comment "TODO"))
   (define-key python-mode-map (kbd "C-c d c t")
     'python-insert-ptyhon-todo-comment)
   (evil-define-key 'motion python-mode-map (kbd "SPC d c t")
     'python-insert-ptyhon-todo-comment)

   (defun python-insert-ptyhon-fixme-comment ()
     (interactive)
     (python-insert-reminder-comment "FIXME"))
   (define-key python-mode-map (kbd "C-c d c f")
     'python-insert-ptyhon-fixme-comment)
   (evil-define-key 'motion python-mode-map (kbd "SPC d c f")
     'python-insert-ptyhon-fixme-comment)

   (defun python-insert-ptyhon-xxx-comment ()
     (interactive)
     (python-insert-reminder-comment "XXX"))
   (define-key python-mode-map (kbd "C-c d c x")
     'python-insert-ptyhon-xxx-comment)
   (evil-define-key 'motion python-mode-map (kbd "SPC d c x")
     'python-insert-ptyhon-xxx-comment)
   ))

(provide 'init-python)
