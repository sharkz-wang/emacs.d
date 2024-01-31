(require-package 'diff-hl)

(defhydra hydra-vcs-menu (:color pink :hint nil :idle 0.3)
  "
^Move^             ^Display^        ^Operations^
------------------------------------------------------
_j_: next          _d_: diff        _r_: revert
_k_: previous                       _s_: stage
"

  ("j" diff-hl-next-hunk)
  ("k" diff-hl-previous-hunk)
  ("d" diff-hl-show-hunk)
  ("s" diff-hl-stage-current-hunk)
  ("r" diff-hl-revert-hunk)

  ("q"   nil "cancel" :color blue)
)

(global-diff-hl-mode 1)
(diff-hl-margin-mode 1)

;; don't highlight staged change
(setq diff-hl-show-staged-changes nil)
;; make room in display-line-number column for margin symbols
(setq display-line-numbers-width 3)

(custom-set-faces
 '(diff-hl-margin-insert ((t :inherit line-number
			     :foreground "#A6E22E" :weight heavy))))
(custom-set-faces
 '(diff-hl-margin-delete ((t :inherit line-number
			     :foreground "#FF8700" :weight heavy))))
(custom-set-faces
 '(diff-hl-margin-change ((t :inherit line-number
			     :foreground "#66D9EF" :weight heavy))))
(custom-set-faces
 '(diff-hl-margin-ignored ((t :inherit line-number
			     :foreground "#747474" :weight heavy))))
(custom-set-faces
 '(diff-hl-margin-unknown ((t :inherit line-number
			     :foreground "#747474" :weight heavy))))

(customize-set-variable
 'diff-hl-margin-symbols-alist '((insert . "+") (delete . "-")
				 (change . "*") (unknown . "?")
				 (ignored . "i")))

;; make diff-hl refresh margins after magit operations
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'magit-post-stage-hook 'diff-hl-magit-post-refresh)

;; FIXME: find a better keybinding
(evil-global-set-key 'normal (kbd "SPC g .") 'hydra-vcs-menu/body)

(provide 'init-vcs)
