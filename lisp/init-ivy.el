(require-package 'ivy)

(add-hook 'ivy-mode-hook
	  (lambda ()
	    (interactive)
	    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
	    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
	    ))

(provide 'init-ivy)
