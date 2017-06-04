(add-hook 'emacs-lisp-mode-hook
	  (lambda ()

	    (require 'init-company)
	    (setq ycmd-global-modes '(not emacs-lisp-mode))

	    (modify-syntax-entry ?- "w")
	    ))

(provide 'init-elisp)
