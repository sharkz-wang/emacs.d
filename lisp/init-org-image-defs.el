
(defun --org-download-recenter-inserted-image ()
    (previous-line)
    (recenter)
)

(defun --increase-org-image-size-at-point ()
    (interactive)
    (--adjust-org-image-size-at-point 10)
)

(defun --increase-org-image-size-proportionally-at-point ()
    (interactive)
    (--adjust-org-image-size-at-point (/ (frame-text-width) 10))
)

(defun --decrease-org-image-size-at-point ()
    (interactive)
    (--adjust-org-image-size-at-point -10)
)

(defun --decrease-org-image-size-proportionally-at-point ()
    (interactive)
    (--adjust-org-image-size-at-point (- 0 (/ (frame-text-width) 10)))
)

(defun --adjust-org-image-size-at-point (gap)
    (interactive)
    (save-excursion
        (or (re-search-backward
              "^#\\+ATTR_ORG:\s+:width\s+[0-9]+"
              (save-excursion (previous-line)
                              (beginning-of-line)
                              (point))
              t)
            (error "No image attribute found."))
        (re-search-forward "[0-9]+" nil t)
        (goto-char (match-beginning 0))
        (replace-match
          (number-to-string
            (+ gap (string-to-number (match-string 0)))))

        (org-display-inline-images)
        (recenter-top-bottom 0)
    )
)

(provide 'init-org-image-defs)
