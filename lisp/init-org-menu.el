(require-package 'hydra)

(require 'init-bookmarked-repos)

(defhydra hydra-org-menu (:color pink :hint nil :exit t)
  "
^Agenda...^        ^Capture...^       ^Open...^
^^^^^^^^---------------------------------------------------------------------------
_a_: agenda        _c_: cauture       _oo_: inbox
^^^^                                  _oq_: quick events
^^^^                                  _oc_: cheatsheet
"
  ;; agenda ...
  ("a" org-agenda)

  ;; capture ...
  ("c" org-capture-force-horizontal)

  ;; open ...
  ("oo" (find-file (concat (file-name-as-directory org-agenda-dir) "inbox.org")))
  ("oq" (find-file (concat (file-name-as-directory org-agenda-dir) "quick.org")))
  ("oc" (find-file (concat (file-name-as-directory org-directory) "cheatsheet.org")))
  )

(evil-global-set-key 'normal (kbd "SPC o") 'hydra-org-menu/body)
(evil-global-set-key 'visual (kbd "SPC o") 'hydra-org-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC o") 'hydra-org-menu/body)

(provide 'init-org-menu)
