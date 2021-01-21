(require 'init-bookmarked-repos)

(defhydra hydra-search-menu (:color pink :hint nil :exit t)
  "
^Buffers...^           ^Files...^              ^Semantics...^
^^^^^-----------------------------------------------------------------
_s_: current buffer    _f_: browse             _o_: outline
_b_: all buffers       _d_: current dir        _O_: all buffers outline
^^                     _m_: bookmarked dir     _j_: dumb-jump
^^                     _/_: current project
"
  ;; buffers
  ("s" helm-occur)
  ("b" helm-do-ag-buffers)
  ;; files
  ("f" helm-do-ag)
  ("d" helm-do-ag-curr-dir)
  ("m" (hydra-bookmarked-repo-menu-action 'helm-do-ag))
  ("/" helm-projectile-ag)
  ;; semantics
  ("o" helm-imenu-no-default)
  ("O" helm-imenu-in-all-buffers-no-default)
  ("j" dumb-jump-go)

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
(evil-define-key evil-magit-state magit-mode-map (kbd "SPC s") 'hydra-search-menu/body)
(evil-define-key 'normal magit-diff-mode-map (kbd "SPC s") 'hydra-search-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC s") 'hydra-search-menu/body)

(provide 'init-search)
