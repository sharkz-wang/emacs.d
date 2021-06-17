(require-package 'compile)
(require 'init-compile-defs)

(defhydra compile-menu (:color pink :hint nil :exit t)
  "
^Compile^               ^Jump to...^          ^Compilation window^
--------------------------------------------------------------------
_c_: compile            _j_: next error       _w_: open
_r_: recompile          _k_: previous error   _x_: close
_e_: edit registers
"
  ("c" run-compile-command-of-register)
  ("r" recompile)
  ("e" set-and-run-register-compile-command)
  ("j" next-error)
  ("k" previous-error)
  ("w" open-compilation-window)
  ("x" delete-compilation-window)

  ("q" nil "cancel" :color blue)
)

;; use smaller font size for high volumn compilation output
(add-hook 'compilation-mode-hook 'init-compilation-mode)
(defun init-compilation-mode ()
    (text-scale-decrease 4)
)

(setq compilation-scroll-output t)

(global-set-key (kbd "C-c c") 'compile-menu/body)
(evil-global-set-key 'normal (kbd "SPC c") 'compile-menu/body)

(provide 'init-compile)
