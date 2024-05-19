(require-package 'hydra)
(require 'dired)

(require 'init-find-files)

(defhydra hydra-project-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Save...^               ^Browse...^                        ^Search...^        ^Operate...^
^^^^^^^^^------------------------------------------------------------------------------
_a_: register project   _f_: current project               _o_: other file    _x_: kill project buffers
_k_: remove project     _d_: dwim current project                             _/_: keyword
_s_: project buffers    _b_: project buffer
^^                      _p_: switch to open project
^^                      _l_: other projects
^^                      _r_: recent files
^^                      _m_: project in teleport
^^                      _TAB_: buffer in prev project
^^                      _`_: buffer in prev prev porject
^^                      _D_: project dirs
"
  ;; save ...
  ("a" projectile-add-current-project)
  ("k" projectile-remove-known-project)
  ("s" projectile-save-project-buffers)
  ;; browse ...
  ("f" helm-projectile-find-file)
  ("d" projectile-find-file-dwim)
  ("b" projectile-switch-to-buffer)
  ("D" projectile-find-dir)
  ("p" --switch-to-open-project-buffer)
  ("l" --switch-to-open-project-buffer)
  ("L" projectile-switch-project)
  ("r" helm-projectile-recentf)
  ("m" (teleport-invoke 'projectile-switch-to-portal))
  ("TAB" projectile-switch-to-prev-project)
  ("`" projectile-switch-to-prev-prev-project)
  ;; search ...
  ("o" helm-projectile-find-other-file)
  ("/" helm-projectile-ag)
  ;; operate ...
  ("x" projectile-kill-buffers)

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

(defun projectile-switch-to-portal (dir)
  (interactive)
  (let ((target-buffer
	 (car (projectile-project-buffers
	       (projectile-project-root dir)))))
    (if target-buffer
	(switch-to-buffer target-buffer)
      (cl-letf ((repo-root (projectile-project-root dir))
		((symbol-function 'projectile-project-root)
		 #'(lambda (&optional ARG) repo-root)))
	(helm-projectile-recentf))

      ))
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

(defun --switch-to-open-project-buffer ()
  (interactive)
  (let ((projectile-switch-project-action 'helm-projectile-switch-to-buffer))
    (projectile-switch-open-project)
    ))

(evil-global-set-key 'normal (kbd "SPC p") 'hydra-project-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC p") 'hydra-project-menu/body)

(provide 'init-project)
