
;; TODO: defcustom it
(setq global-frame-font nil)

(defun --do-nothing (&rest args))

(defun --enable-truncate-lines ()
  (interactive)
  (setq truncate-partial-width-windows nil)
  (cl-letf (((symbol-function 'message) '--do-nothing))
    (toggle-truncate-lines 1)))

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

;; by follow-mode-view, we mean entering follow-mode and automatically
;; open a split-window view because it's sometimes too exhausting to do
;; it yourself
(defun --toggle-follow-mode-view ()
    (interactive)
    (if (bound-and-true-p follow-mode)
      (progn
        (follow-mode -1)
	;; TODO: make it save to original value
	(centered-window-mode 1)
        (delete-other-windows)
	(centered-cursor-mode -1)
      )
      (progn
        (follow-mode 1)
	(centered-window-mode -1)
        (split-window-right)
	(centered-cursor-mode 1)
      )
    )
)

(provide 'init-basics-defs)
