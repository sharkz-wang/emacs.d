(require 'init-bookmarked-repos)

(defhydra hydra-search-menu (:color pink :hint nil :exit t)
  "
^Search^
^^^^^^^^-------------------------
_s_: current buffer
_f_: helm find file
_d_: current dir
_o_: outline
_O_: all buffers outline
_b_: all buffers
_j_: dumb jump
_m_: bookmarked dir
_/_: current project
"
  ("s" helm-occur)
  ("f" helm-do-ag)
  ("d" helm-do-ag-curr-dir)
  ("o" helm-imenu-no-default)
  ("O" helm-imenu-in-all-buffers-no-default)
  ("b" helm-do-ag-buffers)
  ("j" dumb-jump-go)
  ("m" (hydra-bookmarked-repo-menu-action 'helm-do-ag))
  ("/" helm-projectile-ag)

  ("c" nil "cancel" :color blue)
  )

(defun helm-do-ag-curr-dir ()
  (interactive)
  (helm-do-ag (f-dirname (buffer-file-name)))
  )

(require-package 'dumb-jump)
(dumb-jump-mode)
(setq dumb-jump-selector 'helm)

(evil-global-set-key 'normal (kbd "SPC s") 'hydra-search-menu/body)

(provide 'init-search)
