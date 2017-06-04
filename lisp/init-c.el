(add-hook 'c-mode-hook
	  (lambda ()
	    
	    (modify-syntax-entry ?_ "w")
	    
	    (require 'init-semantic)
	    (require 'init-gtags)
	    (require 'init-company)
	    (require 'init-ycmd)
	    
	    ;; auto indenting and pairing curly brace
	    (defun c-mode-insert-lcurly ()
	      (interactive)
	      (insert "{")
	      (let ((pps (syntax-ppss)))
		(when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
		  (c-indent-line)
		  (insert "\n\n}")
		  (c-indent-line)
		  (forward-line -1)
		  (c-indent-line))))
	    (define-key c-mode-base-map "{" 'c-mode-insert-lcurly)
	    ))

(provide 'init-c)
