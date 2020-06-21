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
  ("e" (bookmarked-repo-do-action "~/.emacs.d") "~/.emacs.d\n")
  ("l" (bookmarked-repo-do-action "~/.emacs.d/lisp") "~/.emacs.d/lisp\n")
  ("L" (bookmarked-repo-do-action "~/.emacs.d/elpa") "~/.emacs.d/elpa\n")
  ("k" (bookmarked-repo-do-action "~/src/linux/linux-5.4.81") "kernel\n")
  ("i" (bookmarked-repo-do-action "~/src/linux/linux-5.4.81/include") "kernel/include\n")
  ("d" (bookmarked-repo-do-action "~/src/linux/linux-5.4.81/Documentation") "kernel/Documentation\n")

  ("q" nil "quit" :color blue)
  )

(defun hydra-bookmarked-repo-menu-action (func)
  (interactive)
  (setq bookmarked-repo-action func)
  (hydra-bookmarked-repo-menu/body)
  )

(provide 'init-bookmarked-repos)
