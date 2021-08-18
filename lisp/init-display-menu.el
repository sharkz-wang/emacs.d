;; for frame-wide zooming instead of buffer-wide
(require 'zoom-frm)
(require 'init-display-defs)

(defhydra hydra-display-menu (:color pink :hint nil :idle 0.3)
  "
^Zoom frame ...^       ^Zoom buffer ...^        ^Center area width^
---------------------------------
_n_: zoom in frame     _j_: zoom in buffer      _+_: increase
_p_: zoom out frame    _k_: zoom out buffer     _-_: decrease
_0_: reset zoom
"
  ;; zoom frame ...
  ("p" zoom-in)
  ("n" zoom-out)

  ;; zoom buffer ...
  ("k" (text-scale-adjust +1))
  ("j" (text-scale-adjust -1))

  ;; increase centered-window width
  ("+" --increase-centered-window-size)
  ;; don't bother me if I forget to press shift
  ("=" --increase-centered-window-size)
  ("-" --decrease-centered-window-size)

  ("0" --reset-all-zoom)

  ("q"   nil "cancel" :color blue)
)

(evil-global-set-key 'normal (kbd "SPC z") 'hydra-display-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
    (kbd "SPC z") 'hydra-display-menu/body)

(provide 'init-display-menu)
