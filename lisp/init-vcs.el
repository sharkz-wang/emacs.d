(require-package 'git-gutter)
(require-package 'fringe-helper)
(require 'git-gutter-fringe)
(require-package 'git-gutter)

(defhydra hydra-vcs-menu (:color pink :hint nil)
  "
^Move^             ^Display^        ^Operations^
------------------------------------------------------
_j_: next          _._: fringe      _r_: revert
_k_: previous      _d_: diff        _s_: stage
"
  ;; zoom ...

  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("." toggle-git-gutter-indicator)
  ("d" toggle-git-gutter-popup-diff)
  ("r" git-gutter:revert-hunk)
  ("s" git-gutter:stage-hunk)

  ("q"   nil "cancel" :color blue)
)

;; TODO: move them to `init-vcs-defs.el'
(defun toggle-git-gutter-indicator ()
    (interactive)
    ;; FIXME: hacky trick to force redrawing screen
    (git-gutter:toggle)
    (switch-to-last-buffer)
    (switch-to-last-buffer)
)

(defun toggle-git-gutter-popup-diff ()
    (interactive)
    (if (get-buffer-window "*git-gutter:diff*")
      (delete-window (get-buffer-window "*git-gutter:diff*"))
      (git-gutter:popup-diff)
    )
)

(global-git-gutter-mode 1)

(setq git-gutter:ask-p nil)

(setq git-gutter:added-sign " + ")
(setq git-gutter:deleted-sign " - ")
(setq git-gutter:modified-sign " * ")

;; FIXME: find a better keybinding
(evil-global-set-key 'normal (kbd "SPC g .") 'hydra-vcs-menu/body)

(provide 'init-vcs)
