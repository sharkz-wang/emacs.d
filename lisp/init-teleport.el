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

(define-key teleport-map "1"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "2"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "3"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "4"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "5"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "6"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "7"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "8"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "9"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "0"
	    (lambda () (interactive) (teleport-do-action "~/")))
(define-key teleport-map "="
	    (lambda () (interactive) (teleport-do-action "~/")))

(provide 'init-teleport)
