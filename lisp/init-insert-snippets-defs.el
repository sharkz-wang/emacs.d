
(defun expand-yasnippet-from-keyword (keyword)
  (interactive)
  (indent-according-to-mode)
  (evil-emacs-state)
  (yas-expand-snippet (yas-lookup-snippet keyword))
  )

(defun evilnc-invert-comment-line-by-line (&optional NUM) (interactive "p")
  (setq evilnc-invert-comment-line-by-line t)
  (evilnc-comment-or-uncomment-lines NUM)
  (setq evilnc-invert-comment-line-by-line nil))

(defun comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
	  (setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-region beg end)))

(defun uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
	  (setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(uncomment-region beg end)))

(provide 'init-insert-snippets-defs)
