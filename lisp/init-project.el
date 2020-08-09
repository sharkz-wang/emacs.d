(require-package 'hydra)
(require-package 'git-timemachine)
(require-package 'magit)

(defhydra hydra-project-menu (:color pink :hint nil :exit t)
  "
^Save...^               ^Brose...^                   ^VCS...^                      ^Search...^        ^Operate...^
^^^^^^^^^--------------------------------------------------------------------------------------------------------------
_s_: project buffers    _f_: current project         _gs_:  brief status           _o_: other file    _K_: kill project buffers
^^                      _d_: dwim current project    _gS_:  full status            _/_: keyword
^^                      _b_: project buffers         _gm_:  magit menu
^^                      _D_: project dirs            _gb_:  git blame
^^                      _p_: other projects          _gfu_: diff current file
^^                      _r_: recent files            _gfh_: current file's history
^^^^                                                 _gt_:  git timemachine
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
  ;; vcs ...
  ("gs" magit-status-simplified)
  ("gS" magit-status-full)
  ("gm" magit-dispatch)
  ("gb" magit-blame)
  ("gfh" magit-log-buffer-file)
  ("gfu" magit-diff-buffer-file)
  ("gt" git-timemachine)
  ;; search ...
  ("o" helm-projectile-find-other-file)
  ("/" helm-projectile-ag)
  ;; operate ...
  ("K" projectile-kill-buffers)

  ("c" nil "cancel" :color blue)
  )

(evil-global-set-key 'normal (kbd "SPC p") 'hydra-project-menu/body)

(provide 'init-project)
