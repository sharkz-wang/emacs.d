(require-package 'hydra)
(require-package 'git-timemachine)
(require-package 'magit)
(require 'dired)

(require 'init-find-files)

(defhydra hydra-project-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Save...^               ^Browse...^                          ^VCS...^                      ^Search...^        ^Operate...^
^^^^^^^^^--------------------------------------------------------------------------------------------------------------
_a_: register project   _f_: current project                 _gss_: brief status           _o_: other file    _K_: kill project buffers
_k_: remove project     _d_: dwim current project            _gsm_: brief status on bookmarks ^^              _/_: keyword
_s_: project buffers    _b_: project buffers                 _gSS_: full status
^^                      _D_: project dirs                    _gSM_: full status on bookmarks
^^                      _p_: other projects                  _gg_:  magit menu
^^                      _r_: recent files                    _gm_:  magit menu on bookmarks
^^                      _m_: project in bookmarks            _gb_:  git blame
^^                      _TAB_: buffer in prev project        _gfu_: diff current file
^^                      _`_: buffer in prev prev porject     _gfh_: current file's history
^^^^                                                         _gt_:  git timemachine
"
  ;; save ...
  ("a" projectile-add-current-project)
  ("k" projectile-remove-known-project)
  ("s" projectile-save-project-buffers)
  ;; browse ...
  ("f" find-file-in-project-prompt)
  ("d" projectile-find-file-dwim)
  ("b" projectile-switch-to-buffer)
  ("D" projectile-find-dir)
  ("p" projectile-switch-project)
  ("r" helm-projectile-recentf)
  ("m" (hydra-bookmarked-repo-menu-action 'search-file-in-project))
  ("TAB" projectile-switch-to-prev-project)
  ("`" projectile-switch-to-prev-prev-project)
  ;; vcs ...
  ("gss" magit-status-simplified)
  ("gsm" (hydra-bookmarked-repo-menu-action 'magit-status-simplified-on-path))
  ("gSS" magit-status-full)
  ("gSM" (hydra-bookmarked-repo-menu-action 'magit-status-full-on-path))
  ("gg" magit-dispatch)
  ("gc" magit-quick-stash-all)
  ("gm" (hydra-bookmarked-repo-menu-action 'magit-dispatch-on-path))
  ("gb" magit-blame)
  ("gfh" magit-log-buffer-file)
  ("gfu" magit-diff-buffer-file)
  ("gt" git-timemachine)
  ("gp" magit-open-known-project)
  ;; search ...
  ("o" helm-projectile-find-other-file)
  ("/" helm-projectile-ag)
  ;; operate ...
  ("K" projectile-kill-buffers)

  ("c" nil "cancel" :color blue)
  )

(defun projectile-add-current-project ()
    (interactive)
    (projectile-add-known-project default-directory)
    (message "Directory `%s' added into known projects."
	     default-directory))

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

(defun projectile-switch-to-prev-project ()
  (interactive)
  (switch-to-buffer
   (car (projectile-project-buffers
         (projectile-acquire-root
          (car (projectile-relevant-open-projects))
          )))
   )
  )

(defun projectile-switch-to-prev-prev-project ()
  (interactive)
  (switch-to-buffer
   (car (projectile-project-buffers
         (projectile-acquire-root
          (nth 1 (projectile-relevant-open-projects))
          )))
   )
  )

(defhydra hydra-git-timemachine-menu (:color pink :hint nil :idle 0.3)
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
