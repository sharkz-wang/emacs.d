(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'helm-c-yasnippet)
(require 'init-insert-snippets-defs)

(defhydra hydra-insert-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Snippets^           ^Comment^         ^Argument^
---------------------------------------------------
_i_: from menu       _c_: comments...  _a_: append
_f_: for-loop        _T_: TODO         _s_: select
_p_: print           _F_: FIXME
^ ^                  _X_: XXX
"
  ("i" helm-yas-complete)

  ("f" (funcall insert-for-loop))
  ("p" (funcall insert-print))

  ("c" comment-menu/body)
  ("T" (funcall insert-todo-comment))
  ("F" (funcall insert-fixme-comment))
  ("X" (funcall insert-xxx-comment))

  ("a" (funcall append-argument))
  ("s" (funcall insert-argument-select))

  ("q" nil "cancel" :color blue)
  )

(defhydra comment-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Comments^
---------------------------------------------------
_i_: invert line(s)
_c_: comment line(s)
_u_: uncomment line(s)
_y_: copy and comment line(s)
"

  ;; TODO: insert documented comments by
  ;;       `srecode-document-insert-comment'
  ("i" evilnc-invert-comment-line-by-line)
  ("c" comment-region-or-line)
  ("u" uncomment-region-or-line)
  ("y" evilnc-copy-and-comment-lines)

  ("q" nil "cancel" :color blue)
  )

(yas-global-mode 1)
;; enter `evil-insert-state' right after snippets are inserted
(advice-add 'helm-yas-complete :after #'evil-insert-state)

;; insert snippets by generic semantics
(setq-default insert-for-loop 'nil)
(make-variable-buffer-local 'insert-for-loop)
(setq-default insert-print 'nil)
(make-variable-buffer-local 'insert-print)

;; insert common comments
(setq-default insert-todo-comment 'nil)
(make-variable-buffer-local 'insert-todo-comment)
(setq-default insert-fixme-comment 'nil)
(make-variable-buffer-local 'insert-fixme-comment)
(setq-default insert-xxx-comment 'nil)
(make-variable-buffer-local 'insert-xxx-comment)

;; manipulate argument list
(setq-default append-argument 'nil)
(make-variable-buffer-local 'append-argument)
(setq-default insert-argument-select 'nil)
(make-variable-buffer-local 'insert-argument-select)

(global-set-key (kbd "C-c i") 'hydra-insert-menu/body)
(evil-global-set-key 'normal (kbd "SPC i") 'hydra-insert-menu/body)
(evil-global-set-key 'visual (kbd "SPC i") 'hydra-insert-menu/body)

(provide 'init-insert-snippets)
