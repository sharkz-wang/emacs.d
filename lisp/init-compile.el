(require-package 'compile)
(require 'init-compile-defs)

(defhydra compile-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Compile^               ^Jump to...^          ^Compilation window^
--------------------------------------------------------------------
_c_: compile            _n_: next error       _w_: open
_r_: recompile          _p_: previous error   _x_: close
_k_: kill compilation
_e_: edit registers
"
  ("c" run-compile-command-of-register)
  ("r" recompile)
  ("k" kill-compilation)
  ("e" set-and-run-register-compile-command)
  ("n" next-error)
  ("p" previous-error)
  ("w" open-compilation-window)
  ("x" delete-compilation-window)

  ("q" nil "cancel" :color blue)
)

;; use smaller font size for high volumn compilation output
(add-hook 'compilation-mode-hook 'init-compilation-mode)
(defun init-compilation-mode ()
    (text-scale-decrease 8)
)

(setq compilation-scroll-output t)

(global-set-key (kbd "C-c c") 'compile-menu/body)
(evil-global-set-key 'normal (kbd "SPC c") 'compile-menu/body)

(provide 'init-compile)
