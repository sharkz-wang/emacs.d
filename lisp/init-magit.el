(require-package 'magit)
(require 'magit-log)
(require-package 'git-timemachine)
(require-package 'diff-hl)

(require 'init-magit-defs)

(defhydra hydra-vcs-menu (:color pink :hint nil :idle 0.3)
  "
^Move^             ^Display^        ^Operations^
------------------------------------------------------
_j_: next          _d_: diff        _r_: revert
^^_k_: previous                     _s_: stage
"

  ("j" diff-hl-next-hunk)
  ("k" diff-hl-previous-hunk)
  ("d" diff-hl-show-hunk)
  ("s" diff-hl-stage-current-hunk)
  ("r" diff-hl-revert-hunk)

  ("q"   nil "cancel" :color blue)
)

(defhydra hydra-magit-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^VCS...^                        ^Magit...^              ^Quick...^
----------------------------------------------------------------------
_s_: brief status              _l_: log head           _S_: stash all
_a_: full status               _d_: diff unstaged
_g_: magit menu                _D_: diff staged
_b_: git blame                 _r_: rebase
_f_: file dispatch             _R_: reflog
_t_: git timemachine
_$_: magit-process-buffer
_._: diff hightlight menu
"
  ;; vcs ...
  ("s" magit-status-simplified)
  ("a" magit-status-full)
  ("g" magit-dispatch)
  ("c" magit-quick-stash-all)
  ("b" magit-blame)
  ("f" magit-file-dispatch)
  ("t" git-timemachine)
  ("$" magit-process-buffer)
  ("p" magit-open-known-project)
  ("." hydra-vcs-menu/body)

  ("l" magit-log-head)
  ("d" magit-diff-unstaged)
  ("D" magit-diff-staged)
  ("r" magit-rebase-interactive)
  ("R" magit-reflog-head)

  ("S" magit-quick-stash-all)

  ("c" nil "cancel" :color blue)
  )

;; default arguments for magit-log
(custom-set-variables
 '(magit-log-arguments '("-n32" "--decorate")))

;; set magit popup windows default to full-screen
(setq magit-display-buffer-function
      #'magit-display-buffer-same-window-except-diff-v1)
;; make magit diff buffers always shown in full screen window
(add-to-list 'display-buffer-alist
	     ;; diff buffers (both staged/unstaged)
	     '("magit-diff: .*" display-buffer-same-window)
	     t)

;; explicitly set magit log date format
(setq magit-log-margin
      (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))
;; use date for the last time a commit is modified
(setq magit-log-margin-show-committer-date t)

;; customize ediff merge view into 3-col layout
(setq ediff-show-ancestor t)
(advice-add 'ediff-setup-windows-plain-merge :after
	    '--plain-merge-window-setup-3-col-layout)

;; require `cl-lib' to support following snippet
(require-package 'cl-lib)
(eval-after-load "projectile"
  '(progn
     (setq magit-repository-directories
	   (mapcar (lambda (dir)
		     (substring dir 0 -1))
		   (cl-remove-if-not
		    (lambda (project)
		      (file-directory-p (concat project "/.git")))
		    (projectile-relevant-known-projects))))
     (setq magit-repository-directories-depth 1)
     )
  )

(eval-after-load "magit"
    '(progn
         (setq magit-status-sections-hook-orig
               magit-status-sections-hook)
     ))

(eval-after-load "projectile"
  '(progn (setq magit-repository-directories
		(mapcar (lambda (dir) (cons dir 0))
			(projectile-git-repo-list)))))

;; by default, don't display the slow diff view when
;; editing commit message
(customize-set-variable 'magit-commit-show-diff nil)

;; don't highlight staged change
(setq diff-hl-show-staged-changes nil)
;; make room in display-line-number column for margin symbols
(setq display-line-numbers-width 3)

(custom-set-faces
 '(diff-hl-margin-insert ((t :inherit line-number
			     :foreground "#A6E22E"))))
(custom-set-faces
 '(diff-hl-margin-delete ((t :inherit line-number
			     :foreground "#FF8700"))))
(custom-set-faces
 '(diff-hl-margin-change ((t :inherit line-number
			     :foreground "#66D9EF"))))
(custom-set-faces
 '(diff-hl-margin-ignored ((t :inherit line-number
			     :foreground "#747474"))))
(custom-set-faces
 '(diff-hl-margin-unknown ((t :inherit line-number
			     :foreground "#747474"))))

(customize-set-variable
 'diff-hl-margin-symbols-alist '((insert . "+") (delete . "-")
				 (change . "*") (unknown . "?")
				 (ignored . "i")))

;; make diff-hl refresh margins after magit operations
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'magit-post-stage-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode 1)
(diff-hl-margin-mode 1)

(evil-define-key 'normal
  magit-diff-mode-map (kbd "m") 'evil-set-marker-local-global)
(evil-define-key 'normal
  magit-status-mode-map (kbd "m") 'evil-set-marker-local-global)
(evil-define-key 'normal
  magit-diff-mode-map (kbd "'") 'evil-goto-global-mark-line)
(evil-define-key 'normal
  magit-status-mode-map (kbd "'") 'evil-goto-global-mark-line)

(evil-define-key 'normal
  magit-status-mode-map (kbd "s") 'magit-stage)

(evil-define-key 'normal
  magit-mode-map (kbd "SPC m r") 'magit-diff-toggle-refine-hunk)

(evil-define-key 'normal
  magit-log-mode-map (kbd "TAB") 'magit-diff-show-or-scroll-up)

(evil-global-set-key 'normal (kbd "SPC g") 'hydra-magit-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC g") 'hydra-magit-menu/body)


(evil-add-command-properties #'diff-hl-next-hunk :jump t)
(evil-add-command-properties #'diff-hl-previous-hunk :jump t)

(provide 'init-magit)
