
(require 'init-registered-value-defs)

(setq compile-commane-list '())

(defun set-or-get-register-compile-command (reg)
    (interactive)
    (set-or-get-register-value 'compile-commane-list reg)
)

(defun set-and-run-register-compile-command ()
    (interactive)
    (let ((key (read-key "Press register: ")))
	(set-register-value 'compile-commane-list key)
	(compile (set-or-get-register-compile-command key))
    )
)

(defun run-compile-command-of-register ()
    (interactive)
    (let ((key (read-key "Press register: ")))
        (compile (set-or-get-register-compile-command key))
    )
)

(provide 'init-compile-defs)
