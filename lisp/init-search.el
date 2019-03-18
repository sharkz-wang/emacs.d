(require-package 'transient)

(defmacro define-search-dir-func (name dir)
  (interactive)
  (list 'defun name ()
	(list 'interactive)
	(list 'helm-do-ag dir)))

(define-search-dir-func search-lisp-dir "~/.emacs.d/lisp")
(define-search-dir-func search-emacsd-dir "~/.emacs.d")

(define-transient-command search-bookmarked-dirs ()
 ["Transient and dwim commands"
   [
    ("l" "~/.emacs.d/lisp" search-lisp-dir)
    ("e" "~/.emacs.d"      search-emacsd-dir)
    ]])

(evil-global-set-key 'normal (kbd "SPC s m") 'search-bookmarked-dirs)

(provide 'init-search)
