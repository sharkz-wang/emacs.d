(require-package 'org)

(defun init-org-handler ()
  
  (setq org-startup-indented 1)
  (setq org-clock-sound t)
  (setq org-timer-default-timer 25)

  (evil-leader/set-key
    "aoo" 'org-agenda
    "aoa" 'org-agenda-list
    "aol" 'helm-org-agenda-files-headings
    "ot" 'org-todo-list
    "aoc" 'org-capture
    )
  
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "<") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd ">") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
  
  ;; append same level heading right after current heading
  (define-key org-mode-map (kbd "M-RET") (lambda (arg) (interactive "P")
					   (org-insert-heading-after-current)
					   (end-of-line)
					   (evil-insert-state)
					   (when (equal current-prefix-arg '(4))
					     (org-move-subtree-up))
					   ))
  
  (setq org-capture-templates '(("t" "Todo" entry (file+headline "~/note-system.org" "Tasks")
				 "* TODO %?%i\t%^g\n%T")
				("c" "Trace code note" entry (file+olp "~/gtd.org" "Trace Code")
				 "* %?%i\t%^g\n%T\n[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]\n%c")))

  (add-hook 'org-capture-mode-hook
	    (lambda () (evil-emacs-state)))
  
  (custom-set-faces
   '(org-todo ((t :foreground "#FF1493" :weight bold))))
  )

(add-hook 'org-mode-hook 'init-org-handler)

(provide 'init-org)
