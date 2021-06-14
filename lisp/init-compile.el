(require-package 'compile)
(require 'init-compile-defs)

(defhydra compile-menu (:color pink :hint nil :exit t)
  "
^Compile^
---------------------------------------------------
_c_: compile
_r_: recompile
_e_: edit registers
"
  ("c" run-compile-command-of-register)
  ("r" recompile)
  ("e" set-and-run-register-compile-command)

  ("q" nil "cancel" :color blue)
)

(setq compilation-scroll-output t)

(global-set-key (kbd "C-c c") 'compile-menu/body)
(evil-global-set-key 'normal (kbd "SPC c") 'compile-menu/body)

(provide 'init-compile)
