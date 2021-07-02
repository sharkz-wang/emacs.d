(defun init-cc-handler ()

  (custom-set-variables
   '(c-basic-offset '8))
  (c-set-style "linux")

  (require 'init-semantic)
  (require 'init-gtags)
  (helm-gtags-mode t)
  (require 'init-company)
  
  ;; auto indenting and pairing curly brace
  (defun c-mode-insert-lcurly ()
    (interactive)
    (insert "{")
    (let ((pps (syntax-ppss)))
      ;; when EOL and not in string or comment
      (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps))))
	(c-indent-line)
	(insert "\n\n}")
	(c-indent-line)
	(forward-line -1)
	(c-indent-line))))
  (define-key c-mode-base-map "{" 'c-mode-insert-lcurly)

  (defun choose-c-style ()
    (interactive)
    (helm :sources
	  (helm-build-sync-source "c styles"
	    :candidates (mapcar 'car c-style-alist))))

  (defun change-c-style ()
    (interactive)
    (c-set-style (choose-c-style))
    (setq c-basic-offset 'set-from-style))

  (defun choose-c-offset ()
    (interactive)
    (helm :sources
	  (helm-build-sync-source "c offset"
	    :candidates '(2 4 8 "set-from-style"))))

  (defun change-c-offset ()
    (interactive)
    (let ((indent (choose-c-offset)))
      (if (string-equal indent "set-from-style")
	  (progn
	    (setq c-basic-offset 'set-from-style)
	    (setq tab-width 8))
	(progn
	  (setq c-basic-offset (string-to-number indent))
	  (setq tab-width (string-to-number indent)))
	)))

  (defun toggle-indent-tabs-mode ()
    (interactive)
    (let ((indent (if indent-tabs-mode nil t)))
      (setq indent-tabs-mode indent)
      (message "toggle indent-tab-mode: %s" indent)))

  (defun c-style-selector ()
    (interactive)
    (eval (car (read-from-string
		(format "(%s)"
			(helm :sources
			      (helm-build-sync-source "c style setters"
				:candidates '(change-c-style
					      change-c-offset
					      toggle-indent-tabs-mode))))))))

  (evil-leader/set-key
    "ms" 'c-style-selector
    )
  )

(add-hook 'c-mode-hook 'init-cc-handler)
(add-hook 'c++-mode-hook 'init-cc-handler)

(provide 'init-c)
