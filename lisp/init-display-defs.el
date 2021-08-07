(require 'zoom-frm)

(defun --reset-all-zoom ()
    (interactive)
    (ignore-errors
        (zoom-frm-unzoom)
    )
    (text-scale-adjust 0)
)

(defun --increase-centered-window-size ()
  (interactive)
  (setq cwm-centered-window-width (+ cwm-centered-window-width 5))
)

(defun --decrease-centered-window-size ()
  (interactive)
  (setq cwm-centered-window-width (- cwm-centered-window-width 5))
)

(provide 'init-display-defs)
