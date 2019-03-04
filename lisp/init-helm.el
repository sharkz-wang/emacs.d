(require-package 'helm)
(helm-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'helm-config)

(evil-leader/set-key
  "bb" 'helm-buffers-list
  "rl" 'helm-resume
  "fr" 'helm-recentf
  "fb" 'helm-bookmarks
  "ry" 'helm-show-kill-ring
  )

(global-set-key (kbd "M-SPC") 'helm-buffers-list)

(evil-leader/set-key
  "hl" 'helm-info-elisp
  )

(setq bookmarked-search-directories '())

(defun helm-do-custom-ag ()
  (interactive)
  (helm-do-ag
   (helm :sources
	 (helm-build-sync-source "bookmarked directories"
	   :candidates bookmarked-search-directories)
	 )))

;; default search functions that could be overriden by special major mode functions
(evil-global-set-key 'normal (kbd "SPC s s") 'helm-occur)
(evil-global-set-key 'normal (kbd "SPC s p") 'helm-projectile-ag)
(evil-global-set-key 'normal (kbd "SPC s f") 'helm-do-ag)
(evil-global-set-key
 'normal (kbd "SPC s d")
 (lambda () (interactive) (helm-do-ag (f-dirname (buffer-file-name))))
 )

(require-package 'transient)
(load-file "/Users/sharkz/.emacs.d/elpa/transient-20190219.1346/transient.el")

(defmacro define-search-dir-func (name dir)
  (list 'defun name ()
	(list 'interactive)
	(list 'helm-do-ag dir)))

(define-search-dir-func search-lisp-dir "~/.emacs.d/lisp")
(define-search-dir-func search-emacsd-dir "~/.emacs.d")

(define-transient-command search-bookmarked-dirs ()
 ["Transient and dwim commands"
   [
    ("m" "helm menu"       helm-do-custom-ag)
    ("l" "~/.emacs.d/lisp" search-lisp-dir)
    ("e" "~/.emacs.d"      search-emacsd-dir)
    ]])

(evil-global-set-key 'normal (kbd "SPC s m") 'search-bookmarked-dirs)
(evil-global-set-key 'normal (kbd "SPC s b") 'helm-do-ag-buffers)
(evil-global-set-key 'normal (kbd "SPC s i") 'helm-imenu)
(evil-global-set-key 'normal (kbd "SPC s I") 'helm-imenu-in-all-buffers)

(require 'helm-files) ;; included in package helm
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-l") 'helm-find-files-down-last-level)

(require-package 'projectile)
(require-package 'helm-projectile)
(helm-projectile-on)

(projectile-global-mode 1)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

(define-key evil-normal-state-map (kbd "SPC d") 'helm-projectile-find-file)

(define-key evil-normal-state-map (kbd "SPC \`") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p p") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p r") 'helm-projectile-recentf)
(define-key evil-normal-state-map (kbd "SPC p o") 'helm-projectile-find-other-file)
(define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)

(require-package 'helm-ag)
(define-key evil-normal-state-map (kbd "SPC p a") 'helm-projectile-ag)
(define-key evil-normal-state-map (kbd "SPC h a") 'helm-do-ag)

(evil-define-key 'normal helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump)
(evil-define-key 'normal helm-ag-mode-map (kbd "q") 'quit-window)

(add-hook 'helm-ag-mode-hook
	  (lambda ()
	    (interactive)
	    (read-only-mode -1)
	   ))

(provide 'init-helm)
