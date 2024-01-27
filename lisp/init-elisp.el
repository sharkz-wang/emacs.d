(require 'init-elisp-defs)

(defhydra emacs-lisp-main-mode-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Execute ...^
----------------------------------------------------
_ee_: last sexp
_el_: last sexp
_er_: region
_eb_: buffer
"
  ;; TODO: implement following functions
  ;; _ej_: from cursor
  ;; _ek_: until cursor

  ("ee" eval-last-sexp)
  ("el" eval-last-sexp)
  ("er" eval-region)
  ("eb" eval-buffer)

  ("q" nil "cancel" :color blue)
  )

(evil-define-key 'normal emacs-lisp-mode-map (kbd ",") 'emacs-lisp-main-mode-menu/body)
(evil-define-key 'visual emacs-lisp-mode-map (kbd ",") 'emacs-lisp-main-mode-menu/body)
(global-set-key (kbd "C-c ,") 'emacs-lisp-main-mode-menu/body)

(evil-global-set-key 'normal (kbd "SPC t d") 'toggle-debug-on-error)

(add-hook 'emacs-lisp-mode-hook 'init-emacs-lisp-mode)
(defun init-emacs-lisp-mode ()

  (modify-syntax-entry ?- "w")

  ;; setup major-mode interface functions
  (setq insert-for-loop 'emacs-lisp-insert-for-loop)
  (setq insert-print 'emacs-lisp-insert-print)

  (setq insert-todo-comment 'emacs-lisp-insert-todo-comment)
  (setq insert-fixme-comment 'emacs-lisp-insert-fixme-comment)
  (setq insert-xxx-comment 'emacs-lisp-insert-xxx-comment)

  ;; TODO: implemnt following functions
  ;; (setq append-argument 'emacs-lisp-insert-new-arg)
  ;; (setq insert-argument-select 'emacs-lisp-avy-insert-new-arg)
  ;; end major-mode interface functions
  )

(provide 'init-elisp)
