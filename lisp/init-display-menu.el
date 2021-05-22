;; for frame-wide zooming instead of buffer-wide
(require 'zoom-frm)

(defhydra hydra-display-menu (:color pink :hint nil)
  "
^Zoom...^
---------------------------------
_+_: zoom in
_-_: zoom out
_0_: reset zoom
"
  ;; zoom ...

  ("+" zoom-in)
  ("-" zoom-out)
  ("0" zoom-frm-unzoom)

  ("q"   nil "cancel" :color blue)
  )

(evil-global-set-key 'normal (kbd "SPC z") 'hydra-display-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
    (kbd "SPC z") 'hydra-display-menu/body)

(provide 'init-display-menu)
