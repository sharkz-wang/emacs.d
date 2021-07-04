;; for frame-wide zooming instead of buffer-wide
(require 'zoom-frm)
(require 'init-display-defs)

(defhydra hydra-display-menu (:color pink :hint nil)
  "
^Zoom frame ...^       ^Zoom buffer ...^
---------------------------------
_n_: zoom in frame     _j_: zoom in buffer
_p_: zoom out frame    _k_: zoom out buffer
_0_: reset zoom
"
  ;; zoom frame ...
  ("n" zoom-in)
  ("p" zoom-out)

  ;; zoom buffer ...
  ("j" (text-scale-adjust +1))
  ("k" (text-scale-adjust -1))

  ("0" --reset-all-zoom)

  ("q"   nil "cancel" :color blue)
  )

(evil-global-set-key 'normal (kbd "SPC z") 'hydra-display-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
    (kbd "SPC z") 'hydra-display-menu/body)

(provide 'init-display-menu)
