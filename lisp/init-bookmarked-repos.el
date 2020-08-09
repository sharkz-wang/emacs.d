(defvar bookmarked-repo-action nil)

(defun bookmarked-repo-do-action (path)
  (interactive)
  (let ((default-directory path))
    (funcall bookmarked-repo-action path)))

(defhydra hydra-bookmarked-repo-menu (:color pink :exit t)
  "
^Search^
^^^^^^^^-----------------------------------------------------------------
"
  ("l" (bookmarked-repo-do-action "~/.emacs.d/lisp") "~/.emacs.d/lisp\n")
  ("e" (bookmarked-repo-do-action "~/.emacs.d") "~/.emacs.d\n")
  ("q" nil "quit" :color blue)
  )

(defun hydra-bookmarked-repo-menu-action (func)
  (interactive)
  (setq bookmarked-repo-action func)
  (hydra-bookmarked-repo-menu/body)
  )

(provide 'init-bookmarked-repos)
