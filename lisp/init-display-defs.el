(require 'zoom-frm)

(defun --reset-all-zoom ()
    (interactive)
    (ignore-errors
        (zoom-frm-unzoom)
    )
    (text-scale-adjust 0)
)

(provide 'init-display-defs)
