;; -*- lexical-binding: t; -*-

;; (require-package 'calc-bin)

(defun convert-region-base-m-to-n (m n)
  (lambda (start end)
    (interactive "r")
    (let
	((out
	  (convert-base-m-to-n m n (buffer-substring start end))))
      (delete-region start end)
      (insert out)
      )
    )
  )

(defun convert-region-list-base-m-to-n (m n)
  (lambda (start end)
    (interactive "r")
    (let
	((out
	  (s-join " " (mapcar (lambda (str) (convert-base-m-to-n m n str))
			      (split-string (buffer-substring start end) " ")))))
      (delete-region start end)
      (insert out)
      )
    )
  )

(defun convert-base-m-to-n (m n str)
  (let ((calc-number-radix n)
	(m-digit-num (length str)))
    (format "%s%s"
	    (make-string
	     (- (length (math-format-radix (- (expt m m-digit-num) 1)))
		(length (math-format-radix (string-to-number str m))))
	     ?0)
	    (math-format-radix (string-to-number str m))))
  )

(defun base-keyword (base)
  (if (eq base 10)
      ?d
    (if (eq base 16)
	?h
      (+ base #x30)
      )
    )
  )

(loop for m in '(2 8 10 16)
      do (loop for n in '(2 8 10 16)
	       do
	       ( let ((m-keyword (base-keyword m)) (n-keyword (base-keyword n)))
		 
		 (define-key evil-visual-state-map
		   (kbd (format "SPC i c c %c %c" m-keyword n-keyword))
		   (convert-region-base-m-to-n m n))
		 
		 (define-key evil-visual-state-map
		   (kbd (format "SPC i c l %c %c" m-keyword n-keyword))
		   (convert-region-list-base-m-to-n m n))
		 )))

(defun calc-eval-region (start end)
  (interactive "r")
  (let
      ((out
	(calc-eval (buffer-substring start end))))
    (delete-region start end)
    (insert out)
    )
  )

(define-key evil-visual-state-map (kbd "SPC i c e") 'calc-eval-region)

(provide 'init-calc)
