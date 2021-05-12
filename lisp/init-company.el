(require-package 'company)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-selection-wrap-around t)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  ;; prevent <RET> from inserting newlines when in company popup window
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  )

(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'yasnippet-snippets)

(defun exand-yasnippet-from-keyword (keyword)
  (interactive)
  (indent-according-to-mode)
  (evil-emacs-state)
  (yas-expand-snippet (yas-lookup-snippet keyword))
  )

(require-package 'helm-c-yasnippet)

(defhydra hydra-insert-menu (:color pink :hint nil :exit t)
  "
^Snippets^           ^Comment^         ^Argument^
---------------------------------------------------
_i_: from menu       _T_: TODO         _a_: append
_f_: for-loop        _F_: FIXME        _s_: select
_p_: print           _X_: XXX
"
  ("i" helm-yas-complete)

  ("f" (funcall insert-for-loop))
  ("p" (funcall insert-print))

  ("T" (funcall insert-todo-comment))
  ("F" (funcall insert-fixme-comment))
  ("X" (funcall insert-xxx-comment))

  ("a" (funcall append-argument))
  ("s" (funcall insert-argument-select))

  ("c" nil "cancel" :color blue)
  )

(setq-default insert-for-loop 'nil)
(make-variable-buffer-local 'insert-for-loop)
(setq-default insert-print 'nil)
(make-variable-buffer-local 'insert-print)

(setq-default insert-todo-comment 'nil)
(make-variable-buffer-local 'insert-todo-comment)
(setq-default insert-fixme-comment 'nil)
(make-variable-buffer-local 'insert-fixme-comment)
(setq-default insert-xxx-comment 'nil)
(make-variable-buffer-local 'insert-xxx-comment)

(setq-default append-argument 'nil)
(make-variable-buffer-local 'append-argument)
(setq-default insert-argument-select 'nil)
(make-variable-buffer-local 'insert-argument-select)

(global-set-key (kbd "C-c i") 'hydra-insert-menu/body)
(evil-global-set-key 'normal (kbd "SPC i") 'hydra-insert-menu/body)

(advice-add 'helm-yas-complete :after #'evil-insert-state)

(provide 'init-company)
