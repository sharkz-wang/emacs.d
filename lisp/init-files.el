(require-package 'hydra)

(require 'init-bookmarked-repos)
(require 'init-projectile)

(defhydra hydra-file-menu (:color pink :hint nil :exit t)
  "
^Save...^            ^Browse...^                ^Search...^
^^^^^^^^--------------------------------------------------------------
_s_: current buffer  _f_: current dir          _d_: current dir
^^                   _D_: dired current dir    _/_: bookmarked repos
^^                   _r_: recent files
^^                   _b_: bookmarks
^^                   _m_: bookmarked repos
"
  ;; save ...
  ("s" save-buffer)
  ;; browse ...
  ("f" helm-find-files)
  ("D" dired-curr-dir)
  ("r" helm-recentf)
  ("b" helm-bookmarks)
  ("m" (hydra-bookmarked-repo-menu-action 'helm-find-files-in-dir))
  ;; search ...
  ("d" search-file-in-current-directory)
  ("/" (hydra-bookmarked-repo-menu-action 'search-file-in-directory))

  ("c" nil "cancel" :color blue)
  )

(defun search-file-in-current-directory ()
    (interactive)
    (let ((default-directory (f-dirname (buffer-file-name))))
      (call-interactively 'helm-find)
      ))

(defun search-file-in-directory (dir)
    (interactive)
    (let ((default-directory dir))
      (call-interactively 'helm-find)
      ))

(defun helm-find-files-in-dir (dir)
  (interactive)
  ;; XXX: trailing slash in path matters in `helm-file-files'
  (let ((default-directory (file-name-as-directory dir)))
    (helm-find-files nil)
  ))

(defun dired-curr-dir (arg)
  (interactive "P")
  (dired (f-dirname (buffer-file-name)))
  )

(evil-global-set-key 'normal (kbd "SPC f") 'hydra-file-menu/body)

(provide 'init-files)
