(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

;; TODO: BROKEN: (kbd "s") in visual state in magit-status-mode
;; TODO: exclude newline when executing `viw'
;; TODO: yasnippet for print statement in c, but with line number
;; TODO: visual hint when there's multiple frames
;; TODO: where to put custom yasnippets?
;; TODO: a modern custvar management system
;; TODO: show current files history when committing code in magit
;; TODO: company-mode support for elisp
;; TODO: magit: remove trailing white space automatically
;; TODO: add magit-file-dispatch and other dispatch funcs into hydra-menu
;; TODO: bookmark-jump and org-capture-last-stored

;; don't grant outdated *.elc files, it's too dangerous
(setq load-prefer-newer t)
;; TODO: move it elsewhere better
(setq org-agenda-dir "~/org/agenda")

;; support for copying whole buffer to xclip,
;; useful for pasting code onto leetcode
(setq tweak-enable-xclip-support nil)

;; init files ordered by importance
;; determined by:
;;     - basic edior usage
;;     - essentials when init files are broken
;;     - what I use a lot for development
;;     - other helpers
;; foundamentals
(require 'init-package)

(require 'init-basics)
(require 'init-appearance)
;; TODO: combine it to proper file
(require 'init-display-menu)
;; basic buffer/file selection
(require 'init-helm)
(require 'init-ivy)
;; for info look-up
(require 'init-help)
(require 'init-search)
;; so I could fix things faster
(require 'init-elisp)
(require 'init-company)
;; more info to lookup
(require 'init-org)
(require 'init-org-menu)
;; fix things even faster
(require 'init-insert-snippets)
(require 'init-project)
(require 'init-magit)
(require 'init-compile)
(require 'init-ediff)
;; advanced settings for my core modes
(require 'init-gtags)
(require 'init-c)
(require 'init-python)
;; some other settings
(require 'init-persp)
(require 'init-teleport)
(require 'init-treemacs)
(require 'init-dired)
(require 'init-kernel-dev)
(provide 'init-aosp)
(require 'init-external-share-menu)
(require 'init-centaur-tabs)
(require 'init-misc)
;; some advance language features
;; (require 'init-python-advanced)

(require 'cust-vars)

(require 'staging)
