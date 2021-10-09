(require 'init-org-image-defs)

(require-package 'hydra)

(require-package 'org-download)
(require 'org-download)

(defhydra hydra-org-image-menu (:color pink :hint nil :exit nil :idle 0.3)
"
^Insert^                     ^resize^
^^^^^^^^---------------------------------------------------------------------------
_i_: from clipboard          _+_: increase
^^                           _-_: decrease
^^                           _p_: increase in step
^^                           _n_: decrease in step
"
  ;; insert ...
  ("i" org-download-clipboard)

  ;; resize ...
  ("+" --increase-org-image-size-at-point)
  ;; don't bother me if I forget to press shift
  ("=" --increase-org-image-size-at-point)
  ("-" --decrease-org-image-size-at-point)
  ;; increase a step that is propotional to frame size
  ("p" --increase-org-image-size-proportionally-at-point)
  ("n" --decrease-org-image-size-proportionally-at-point)

  ("q" nil "cancel" :color blue)
)

(customize-set-variable 'org-download-image-attr-list
                        '("#+ATTR_ORG: :width 300"))

(advice-add 'org-download-clipboard :after #'--org-download-recenter-inserted-image )

(provide 'init-org-image-menu)
