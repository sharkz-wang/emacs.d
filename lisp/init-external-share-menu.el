(require 'init-external-share-menu-defs)

(defhydra external-share-menu (:color pink :hint nil :idle 0.3 :exit t)
  "
^Genernal^
---------------------------------
_y_: copy file location
_Y_: copy bootlin url
"

  ("y" --copy-file-location)
  ("Y" --copy-elixir-bootlin-url)

  ("q"   nil "cancel" :color blue)
)

(evil-global-set-key 'normal (kbd "SPC O") 'external-share-menu/body)

(provide 'init-external-share-menu)
