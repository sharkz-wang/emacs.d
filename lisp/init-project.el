(require-package 'hydra)

(defhydra hydra-project-menu (:color pink :hint nil :exit t)
  "
^Save...^               ^Brose...^                   ^Search...^        ^Operate...^
^^^^^^^^-----------------------------------------------------------------------------------------
_s_: project buffers    _f_: current project         _o_: other file    _K_: kill project buffers
^^                      _d_: dwim current project    _/_: keyword
^^                      _b_: project buffers
^^                      _D_: project dirs
^^                      _p_: other projects
^^                      _r_: recent files
"
  ;; save ...
  ("s" projectile-save-project-buffers)
  ;; browse ...
  ("f" helm-projectile-find-file)
  ("d" projectile-find-file-dwim)
  ("b" projectile-switch-to-buffer)
  ("D" projectile-find-dir)
  ("p" helm-projectile-switch-project)
  ("r" helm-projectile-recentf)
  ;; search ...
  ("o" helm-projectile-find-other-file)
  ("/" helm-projectile-ag)
  ;; operate ...
  ("K" projectile-kill-buffers)

  ("c" nil "cancel" :color blue)
  )

(evil-global-set-key 'normal (kbd "SPC p") 'hydra-project-menu/body)

(provide 'init-project)
