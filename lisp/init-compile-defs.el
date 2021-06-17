
(require 'init-registered-value-defs)

(setq compile-commane-list '())

(defun open-compilation-window ()
    (interactive)
    (set-window-buffer (split-window-right) (compilation-find-buffer))
)

(defun delete-compilation-window ()
    (interactive)
    (let ((comp-window (get-buffer-window (compilation-find-buffer))))
        (when comp-window
          (delete-window (get-buffer-window (compilation-find-buffer))) 
        )
    )
)

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
