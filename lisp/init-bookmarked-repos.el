(defvar bookmarked-repo-action nil)

(defun bookmarked-repo-do-action (path)
  (interactive)
  (let ((default-directory (file-name-directory path)))
    (funcall bookmarked-repo-action path)))

(defhydra hydra-bookmarked-repo-menu (:color pink :exit t)
  "
^Bookmarked locations^
^^^^^^^^-----------------------------------------------------------------
"
  ("e" (bookmarked-repo-do-action "~/.emacs.d") "~/.emacs.d\n")
  ("l" (bookmarked-repo-do-action "~/.emacs.d/lisp") "~/.emacs.d/lisp\n")
  ("L" (bookmarked-repo-do-action "~/.emacs.d/elpa") "~/.emacs.d/elpa\n")
  ("k" hydra-kernel-repo-menu/body "kernel\n")

  )

(defhydra hydra-kernel-repo-menu (:color pink :exit t)
  "
^Kernel codebase^
^^^^^^^^-----------------------------------------------------------------
"
  ("k" (bookmarked-repo-do-action linux-repo-dir-path)
       "kernel\n")
  ("i" (bookmarked-repo-do-action (concat-path linux-repo-dir-path "include"))
       "kernel/include\n")
  ("d" (bookmarked-repo-do-action (concat-path linux-repo-dir-path "Documentation"))
       "kernel/Documentation\n")

  ("s" hydra-kernel-scheduler-menu/body "scheduler\n")
  ("p" hydra-kernel-perf-events-menu/body "perf_events\n")

  ("q" nil "quit" :color blue)
  )

(defhydra hydra-kernel-scheduler-menu (:color pink :exit t)
  "
^Scheduler source files^
^^^^^^^^-----------------------------------------------------------------
"
  ("s" (bookmarked-repo-do-action (concat-path linux-repo-dir-path
					       "kernel" "sched"))
       "subsystem folder\n")
  ("f" (bookmarked-repo-do-action (concat-path linux-repo-dir-path
					       "kernel" "sched" "fair.c"))
       "fcs scheduler\n")
  )

(defhydra hydra-kernel-perf-events-menu (:color pink :exit t)
  "
^Perf_events source files^
^^^^^^^^-----------------------------------------------------------------
"
  ("a" (bookmarked-repo-do-action (concat-path linux-repo-dir-path
					       "kernel" "events" "core.c"))
       "framework layer\n")
  ("s" (bookmarked-repo-do-action (concat-path linux-repo-dir-path
					       "drivers" "perf" "arm_pmu.c")
       "ARM family hal layer\n")
  ("d" (bookmarked-repo-do-action (concat-path linux-repo-dir-path
					       "arch" "arm64"
					       "kernel" "perf_event.c"))
       "ARMv8 driver\n")

  ("q" nil "quit" :color blue)
  )

(defun hydra-bookmarked-repo-menu-action (func)
  (interactive)
  (setq bookmarked-repo-action func)
  (hydra-bookmarked-repo-menu/body)
  )

(provide 'init-bookmarked-repos)
