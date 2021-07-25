
(defhydra hydra-org-mode-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Operations ...^
---------------------------------
_,_: dwim
_r_: refile
"

  ("," org-capture-finalize)
  ("r" org-capture-refile)

  ("q" nil "cancel" :color blue)
)

(evil-define-key 'normal org-capture-mode-map (kbd ",") 'hydra-org-mode-menu/body)
(evil-define-minor-mode-key 'normal 'org-capture-mode-map (kbd ",") 'hydra-org-mode-menu/body)

(provide 'init-org-capture)
