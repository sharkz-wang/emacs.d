(require 'init-bookmarked-repos)

(defhydra hydra-file-menu (:color pink :hint nil :exit t)
  "
^Find files^
^^^^^^^^-------------------------
_s_: save buffer
_f_: find files
_r_: recent files
_b_: bookmarks
_m_: bookmarked repos
"
  ("s" save-buffer)
  ("f" helm-find-files)
  ("d" dired-curr-dir)
  ("r" helm-recentf)
  ("b" helm-bookmarks)
  ("m" (hydra-bookmarked-repo-menu-action 'helm-find-files-in-dir))

  ("c" nil "cancel" :color blue)
  )

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
