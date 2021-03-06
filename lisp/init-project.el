(require-package 'hydra)
(require-package 'git-timemachine)
(require-package 'magit)
(require 'dired)

(require 'init-find-files)

(defhydra hydra-project-menu (:color pink :hint nil :exit t)
  "
^Save...^               ^Browse...^                  ^VCS...^                      ^Search...^        ^Operate...^
^^^^^^^^^--------------------------------------------------------------------------------------------------------------
_s_: project buffers    _f_: current project         _gss_: brief status           _o_: other file    _K_: kill project buffers
^^                      _d_: dwim current project    _gsm_: brief status on bookmarks ^^              _/_: keyword
^^                      _b_: project buffers         _gSS_: full status
^^                      _D_: project dirs            _gSM_: full status on bookmarks
^^                      _p_: other projects          _gg_:  magit menu
^^                      _r_: recent files            _gm_:  magit menu on bookmarks
^^                      _m_: project in bookmarks    _gb_:  git blame
^^^^                                                 _gfu_: diff current file
^^^^                                                 _gfh_: current file's history
^^^^                                                 _gt_:  git timemachine
"
  ;; save ...
  ("s" projectile-save-project-buffers)
  ;; browse ...
  ("f" find-file-in-project-prompt)
  ("d" projectile-find-file-dwim)
  ("b" projectile-switch-to-buffer)
  ("D" projectile-find-dir)
  ("p" helm-projectile-switch-project)
  ("r" helm-projectile-recentf)
  ("m" (hydra-bookmarked-repo-menu-action 'search-file-in-project))
  ;; vcs ...
  ("gss" magit-status-simplified)
  ("gsm" (hydra-bookmarked-repo-menu-action 'magit-status-simplified-on-path))
  ("gSS" magit-status-full)
  ("gSM" (hydra-bookmarked-repo-menu-action 'magit-status-full-on-path))
  ("gg" magit-dispatch)
  ("gm" (hydra-bookmarked-repo-menu-action 'magit-dispatch-on-path))
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

(defun search-file-in-current-project ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (call-interactively 'helm-find)
      ))

(defun search-file-in-project (dir)
    (interactive)
    (let ((default-directory (projectile-project-root dir)))
      (call-interactively 'helm-find)
      ))

(defhydra hydra-git-timemachine-menu (:color pink :hint nil)
  "
^Revision...^              ^Operate...^
^^^^^^^^^--------------------------------------------------------------------------------------------------------------
_n_: next revision         _y_: copy hash id
_p_: previous revision     _b_: blame
"
  ;; revision
  ("n" git-timemachine-show-next-revision)
  ("p" git-timemachine-show-previous-revision)
  ;; operate
  ("y" git-timemachine-kill-revision)
  ("b" git-timemachine-blame)

  ("c" git-timemachine-quit "cancel" :color blue)
  )

(defun git-timemachine-trasient-state ()
  (interactive)
  ;; FIXME: not automatically showing transient menu
  (hydra-git-timemachine-menu/body)
  (git-timemachine)
  )

(evil-global-set-key 'normal (kbd "SPC p") 'hydra-project-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC p") 'hydra-project-menu/body)

(provide 'init-project)
