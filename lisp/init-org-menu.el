(require-package 'hydra)

(require 'init-org-image-menu)
(require 'init-teleport)

(defhydra hydra-org-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Agenda...^        ^Capture...^       ^Image^         ^Open...^
^^^^^^^^---------------------------------------------------------------------------
_q_: agenda        _c_: cauture       _z_: resize     _TAB_: inbox sidebar
^^^^^^                                                _a_: quick events
^^^^^^                                                _i_: inbox
^^^^^^                                                _oc_: cheatsheet
"
  ;; agenda ...
  ("q" org-agenda)

  ;; capture ...
  ("c" org-capture-force-horizontal)

  ;; image ...
  ("z" hydra-org-image-menu/body)

  ;; open ...
  ("TAB" (org-sidebar-tree-toggle-buffer))
  ("a" (find-file (concat (file-name-as-directory org-agenda-dir) "quick.org")))
  ("i" (find-file (concat (file-name-as-directory org-agenda-dir) "inbox.org")))
  ("oc" (find-file (concat (file-name-as-directory org-directory) "cheatsheet.org")))
  )

(evil-global-set-key 'normal (kbd "SPC o") 'hydra-org-menu/body)
(evil-global-set-key 'visual (kbd "SPC o") 'hydra-org-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC o") 'hydra-org-menu/body)

(provide 'init-org-menu)
