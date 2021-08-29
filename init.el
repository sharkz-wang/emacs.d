(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

;; TODO: how to turn off loading of .cust-vars.el
;; TODO: enlarge `undo-tree-limit'
;; TODO: check-in frequently used icons
;; TODO: better ways to manage customize-group
;; TODO: backup folder for elpa packages
;; TODO: python automation server
;; TODO: pinned buffers
;; TODO: show current files history when committing code in magit
;; TODO: company-mode support for elisp
;; TODO: using undo/redo as hook to run script
;; TODO: magit: remove trailing white space automatically
;; TODO: add magit-file-dispatch and other dispatch funcs into hydra-menu
;; TODO: bookmark-jump and org-capture-last-stored
;; TODO: make `helm-find-files' support whitespace delimiter
;; TODO: make org-refile compatible to plain type records
;; TODO: add `package-refresh-contents' into regular flow

;; TODO: move it elsewhere better
;; don't grant outdated *.elc files, it's too dangerous
(setq load-prefer-newer t)
;; TODO: move it elsewhere better
(setq org-agenda-dir "~/org/agenda")

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
(require 'init-c)
(require 'init-python)
;; some other settings
(require 'init-dired)
(require 'init-ebook)
(require 'init-kernel-dev)
(require 'init-external-share-menu)
(require 'init-misc)
;; recently unused features
;;;; (require 'init-dash)
;;;; (require 'init-uml)
;;;; (require 'init-hex-file)
;;;; (require 'init-image)

(require 'cust-vars)

(require 'staging)
