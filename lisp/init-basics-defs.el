
;; TODO: defcustom it
(setq global-frame-font nil)

(defun new-frame-set-font (new-frame)
    (select-frame new-frame)
    (set-frame-font global-frame-font))

(defun set-global-frame-font (font)
    (interactive)
    (setq global-frame-font font)
    (set-frame-font global-frame-font)
    (unless (member 'new-frame-set-font
		    after-make-frame-functions)
      (add-hook 'after-make-frame-functions
		#'new-frame-set-font))
)

(defun concat-path (&rest sequence)
  (cl-reduce
      (lambda (dir file)
          (concat (file-name-as-directory dir)
		  file))
      sequence
  )
)

(provide 'init-basics-defs)
