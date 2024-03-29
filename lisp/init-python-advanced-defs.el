
(defun flymake-pylint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	;; TODO: make it customizable
	(list "/usr/local/bin/epylint" (list local-file))))

(defun elpy-flymake-error-at-point ()
  "Return the flymake error at point, or nil if there is none."
  (mapconcat #'flymake-diagnostic-text (flymake-diagnostics (point)) "\n"))

(defun python-open-shell-buffer ()
       (interactive)
       (split-window-below)
       (other-window 1)
       (switch-to-buffer "*Python*")
       (other-window 1))

(defun ipython-shell-restart ()
  (interactive)
  (when (comint-check-proc "*Python*")
    (python-shell-clear)
    (elpy-shell-kill))
  (run-python))

;; helper variable for `elpy-shell-send-expression'
(setq python-expr-for-shell nil)
(defun elpy-shell-send-expression ()
  (interactive)
  (if (region-active-p)
      (progn
	(setq python-expr-for-shell
	      (buffer-substring-no-properties
	       (region-beginning) (region-end)))
	(elpy-shell-send-region-or-buffer))
    (when python-expr-for-shell
      (python-shell-send-string (concat python-expr-for-shell "\n")))
    ))


(defun python-shell-clear ()
  (interactive)
  (switch-to-buffer "*Python*")
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer))
  (switch-to-last-buffer))

;; exclusive to current cursor line
(defun elpy-shell-send-until-cursor-pos ()
  (interactive)
  (let ((prev-pos (point)))
    ;; create region from the beginning of buffer to
    ;; the end of current line
    (push-mark (point-min))
    (previous-line)
    (goto-char (line-end-position))
    (activate-mark)

    (when (region-active-p)
      (elpy-shell-send-region-or-buffer))

    ;; revert to previous states
    (deactivate-mark)
    (goto-char prev-pos)
    (pop-mark)
    )
  )

;; inclusive to current cursor line
(defun elpy-shell-send-from-cursor-pos ()
  (interactive)
  (let ((prev-pos (point)))
    ;; create region from the beginning of current line to
    ;; the end of buffer
    (push-mark (line-beginning-position))
    (goto-char (point-max))
    (activate-mark)

    (when (region-active-p)
      (elpy-shell-send-region-or-buffer))

    ;; revert to previous states
    (deactivate-mark)
    (goto-char prev-pos)
    (pop-mark)
    )
  )

(defun elpy-shell-send-paragraph ()
  (interactive)
  (let ((prev-pos (point)))
    ;; create region marking current paragraph
    (backward-paragraph)
    (push-mark (point))
    (forward-paragraph)
    (activate-mark)

    (when (region-active-p)
      (elpy-shell-send-region-or-buffer))

    ;; revert to previous states
    (deactivate-mark)
    (goto-char prev-pos)
    (pop-mark)
    )
  )

(provide 'init-python-advanced-defs)
