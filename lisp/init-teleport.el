(defvar teleport-action nil)
(defvar teleport-map (make-sparse-keymap))

(defun teleport-do-action (path)
  (interactive)
  (let ((default-directory (file-name-directory path)))
    (funcall teleport-action path)))

(defun teleport-invoke (func)
  (interactive)
  (setq teleport-action func)
  (set-transient-map teleport-map))

(require-package 'helm)
(require 'helm-bookmark)
(define-key helm-bookmark-map (kbd "C-c d") 'helm-bookmark-run-delete)
(define-key helm-bookmark-map (kbd "C-c E") 'bookmark-edit-annotation)
(define-key helm-bookmark-map (kbd "C-c e") 'helm-bookmark-run-rename)

(helm-make-command-from-action helm-bookmark-run-edit-annotation
  "Edit bookmark annotation" 'bookmark-edit-annotation)

(helm-make-command-from-action helm-bookmark-run-rename
  "Rename bookmark" 'helm-bookmark-rename)

(define-key teleport-map "D"
	    (lambda () (interactive) (teleport-do-action "~/.dotfiles")))
(define-key teleport-map "e"
	    (lambda () (interactive) (teleport-do-action "~/.emacs.d/init.el")))
(define-key teleport-map "l"
	    (lambda () (interactive) (teleport-do-action "~/.emacs.d/lisp")))

(provide 'init-teleport)
