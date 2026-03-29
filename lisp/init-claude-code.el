
(straight-use-package '(claude-code-ide :type git :host github :repo "manzaltu/claude-code-ide.el"))
(require-package 'eat)

(setq claude-code-ide-use-side-window nil)
(custom-set-variables '(claude-code-ide-terminal-backend 'eat))

(defun --my-claude-code-ide-send-todo-fix-request ()
  (interactive)
  (claude-code-ide-send-prompt
   (format "complete my todo comment in-place at file location %s" (--my-get-loc-string)))
  )

(defun --my-get-loc-string ()
  (let ((ret-list (git-get-file-location-info)))
    (format "%s:%s" (nth 2 ret-list) (nth 3 ret-list))))

(defun my-claude-code-ide-switch-to-buffer ()
  (interactive)
  (claude-code-ide-switch-to-buffer)
  (delete-other-windows)
  (evil-emacs-state)
  )

(defun --my-claude-code-ide-do-not-focus-cursor (func &rest args)
  (interactive)
  (let ((original-window (selected-window)))
    (apply func args)
    (select-window original-window)))

(advice-add 'claude-code-ide :around #'--my-claude-code-ide-do-not-focus-cursor)

(add-hook 'eat-mode-hook
	  (lambda ()
	    (display-line-numbers-mode -1)
	    (evil-emacs-state)
	    (evil-define-key 'emacs eat-mode-map (kbd "C-w") #'eat-self-input)
	    ;; maybe-exit-kj and maybe-exit-jk stinks in eat-mode, let's mute it
	    (evil-define-key 'emacs eat-mode-map "j" #'eat-self-input)
	    (evil-define-key 'emacs eat-mode-map "k" #'eat-self-input)
	    ))

(evil-global-set-key 'normal (kbd "SPC [ [") 'claude-code-ide)
(evil-global-set-key 'normal (kbd "SPC [ b") 'my-claude-code-ide-switch-to-buffer)
(evil-global-set-key 'normal (kbd "SPC [ TAB") 'claude-code-ide-toggle)
(evil-global-set-key 'normal (kbd "SPC [ j") 'claude-code-ide-send-prompt)
(evil-global-set-key 'normal (kbd "SPC [ d") '--my-claude-code-ide-send-todo-fix-request)

(provide 'init-claude-code)
