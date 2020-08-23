(require-package 'hydra)
(require-package 'git-timemachine)
(require-package 'magit)
(require 'dired)

(defhydra hydra-project-menu (:color pink :hint nil :exit t)
  "
^Save...^               ^Browse...^                  ^VCS...^                      ^Search...^        ^Operate...^
^^^^^^^^^--------------------------------------------------------------------------------------------------------------
_s_: project buffers    _f_: current project         _gss_: brief status           _o_: other file    _K_: kill project buffers
^^                      _d_: dwim current project    _gsm_: brief status on bookmarks ^^              _/_: keyword
^^                      _b_: project buffers         _gSS_: full status
^^                      _D_: project dirs            _gSM_: full status on bookmarks
^^                      _p_: other projects          _gm_:  magit menu
^^                      _r_: recent files            _gM_:  magit menu on bookmarks
^^                      _m_: project in bookmarks    _gb_:  git blame
^^^^                                                 _gfu_: diff current file
^^^^                                                 _gfh_: current file's history
^^^^                                                 _gt_:  git timemachine
"
  ;; save ...
  ("s" projectile-save-project-buffers)
  ;; browse ...
  ("f" search-file-in-current-project)
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
  ("gm" magit-dispatch)
  ("gM" (hydra-bookmarked-repo-menu-action 'magit-dispatch-on-path))
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

(evil-global-set-key 'normal (kbd "SPC p") 'hydra-project-menu/body)
(evil-define-key 'normal dired-mode-map (kbd "C-c p") 'hydra-project-menu/body)

(provide 'init-project)
