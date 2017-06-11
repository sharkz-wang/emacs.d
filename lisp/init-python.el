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
   (require 'init-ycmd)

   (setq-default flymake-no-changes-timeout '2)

   (eval-after-load
       'flymake
     (lambda ()
       (defun flymake-pylint-init ()
	 (let* ((temp-file (flymake-init-create-temp-buffer-copy
			    'flymake-create-temp-inplace))
		(local-file (file-relative-name
			     temp-file
			     (file-name-directory buffer-file-name))))
	   (list "/usr/local/bin/epylint" (list local-file))))
       (add-to-list 'flymake-allowed-file-name-masks
		    '("\\.py\\'" flymake-pylint-init)))
     )
   
   (defun show-fly-err-at-point ()
     "If the cursor is sitting on a flymake error, display the message in the minibuffer"
     (require 'cl)
     (interactive)
     (let ((line-no (line-number-at-pos)))
       (dolist (elem flymake-err-info)
	 (if (eq (car elem) line-no)
	     (let ((err (car (second elem))))
	       (message "%s" (flymake-ler-text err)))))))

   (add-hook 'post-command-hook 'show-fly-err-at-point)

   (evil-define-key 'normal python-mode-map (kbd "SPC SPC g j") 'flymake-goto-next-error)
   (evil-define-key 'normal python-mode-map (kbd "SPC SPC g k") 'flymake-goto-prev-error)
   
   (flymake-mode 1)
   
   (require-package 'evil-indent-textobject)

   (define-key evil-normal-state-map (kbd "SPC p 3")
     (lambda
       ()
       (interactive)
       (split-window-right)
       (other-window 1)
       (switch-to-buffer "*Python*")
       (other-window 1)))

   (define-key python-mode-map (kbd "C-c d f")
     (lambda () (interactive)
       (exand-yasnippet-from-keyword "for ... in ... : ...")
       ))

   (define-key python-mode-map (kbd "C-c d p")
     (lambda () (interactive)
       (exand-yasnippet-from-keyword "print")
       ))

   (defun python-insert-formated-string ()
     (interactive)
     (insert "\"\" % ()")
     (left-char 6)
     (evil-insert-state)
     (indent-according-to-mode))
   (evil-define-key 'motion python-mode-map (kbd "SPC d s") 'python-insert-formated-string)

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
