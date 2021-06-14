
(setq sample-list '())

(defun --matched-register (seq reg)
    (cdr (nth 0 (-filter (lambda (pair) (equal reg (car pair))) (symbol-value seq)))))

(defun set-register-value (seq reg)
    (let ((val (read-from-minibuffer
                   (format "Value for for register ?%c: " reg)
		   (--matched-register seq reg))))
        (set seq
             (delete-if (lambda (x) (equal (car x) "c")) (symbol-value seq)))
        (add-to-list seq
                     (cons reg val))
	val
    )
)

(defun set-or-get-register-value (seq reg)
    (let ((val (--matched-register seq reg)))
        (if (equal val nil)
	    (set-register-value seq reg)
	    val
	 )
    )
)

(provide 'init-registered-value-defs)
