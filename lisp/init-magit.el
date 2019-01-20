;; TODO: load magit only since first time in git repo
(require-package 'magit)

;; set magit popup windows default to full-screen
(setq magit-display-buffer-function
	  #'magit-display-buffer-fullframe-status-v1)
;; explicitly set magit log date format
(setq magit-log-margin (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

(require-package 'evil-magit)
(evil-magit-init)

;; require `cl' to support following snippet
(require-package 'cl)
(require 'cl)
(eval-after-load "projectile"
  '(progn
     (setq magit-repository-directories
	   (mapcar (lambda (dir)
		     (substring dir 0 -1))
		   (remove-if-not
		    (lambda (project)
		      (file-directory-p (concat project "/.git")))
		    (projectile-relevant-known-projects))))
     (setq magit-repository-directories-depth 1)
     )
  )

(evil-define-key evil-magit-state magit-mode-map "=" 'magit-diff-less-context)
(define-key magit-log-mode-map (kbd "TAB") 'magit-cycle-margin-style)

;; TODO: move it to init-git.el
(require-package 'git-gutter)

(global-git-gutter-mode 1)

(setq git-gutter:ask-p nil)

(setq git-gutter:added-sign " + ")
(setq git-gutter:deleted-sign " - ")
(setq git-gutter:modified-sign " * ")

(evil-leader/set-key
  "g.s" 'git-gutter:stage-hunk
  "g.d" 'git-gutter:popup-diff
  "g.r" 'git-gutter:revert-hunk
  "g.n" 'git-gutter:next-hunk
  "g.j" 'git-gutter:next-hunk
  "g.p" 'git-gutter:previous-hunk
  "g.k" 'git-gutter:previous-hunk
  )

(require-package 'git-timemachine)
(add-hook
 'git-timemachine-mode-hook
 (lambda
   ()
   ;; XXX: using evil-normal-state will pollute key-bindings in other modes
   (evil-motion-state)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "p") 'git-timemachine-show-previous-revision)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "n") 'git-timemachine-show-next-revision)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "q") 'git-timemachine-quit)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "w") 'git-timemachine-kill-abbreviated-revision)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "W") 'git-timemachine-kill-revision)
   (evil-define-minor-mode-key 'motion 'git-timemachine-mode-map (kbd "b") 'git-timemachine-blame)))


(evil-leader/set-key
  "gs" 'magit-status
  "gm" 'magit-dispatch-popup
  "gb" 'magit-blame
  "gfh" 'magit-log-buffer-file
  "gt" 'git-timemachine
  )

(add-hook 'magit-popup-mode-hook
	  (lambda ()
	    (setq evil-default-state 'emacs)
	    ))

(provide 'init-magit)
