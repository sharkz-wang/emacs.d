
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

;; simply a handy version of `query-replace' which supports `thing-at-point'
(defun --query-replace-thing-at-point ()
    (interactive)
    (if (thing-at-point 'word)
      (let* ((from-str (thing-at-point 'word))
             (to-str   (read-from-minibuffer (format "Query replace %s with: "
						     from-str)
                                             from-str)))
        (save-excursion
          (beginning-of-buffer)
          (query-replace from-str to-str))
      )
      (call-interactively 'query-replace)
    )
)

(defun --evil-insert-state-after-yas-complete (func &rest r)
  ;; don't do anything if `helm-yas-complete' was aborted by C-g
  (when (apply func r)
    (evil-insert-state)
    )
  )

(provide 'init-insert-snippets-defs)
