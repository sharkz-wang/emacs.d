(defun --org-capture-get-file-line-num ()
    (with-current-buffer (org-capture-get :original-buffer)
      (if (region-active-p)
        (number-to-string (line-number-at-pos (region-beginning)))
        (number-to-string (line-number-at-pos (point)))
      )
    )
)

(defun --org-capture-get-buffer-babel-lang ()
    (with-current-buffer (org-capture-get :original-buffer)
        (substring (format "%s" major-mode) 0 -5)
    )
)

(defun --org-capture-get-region-code-fragment ()
  (string-trim
    (with-current-buffer (org-capture-get :original-buffer)
      (let ((code-frag (if (region-active-p)
                           (buffer-substring-no-properties
                               (region-beginning) (region-end))
                           (thing-at-point 'line))))
        ;; no need to keep mark once we finish capturing
        (deactivate-mark)
        (format "%s\n%s%s%s...%s%s\n%s"
                (save-excursion
                  (beginning-of-defun)
                  (buffer-substring-no-properties
                      (point) (point-at-eol))
                )
                (substring code-frag 0
                           (string-match "[^ \t]+" code-frag))
                comment-start
                comment-padding
                comment-padding
                comment-end
                code-frag
        )
      )
    )
  )
)

(provide 'init-org-capture-defs)
