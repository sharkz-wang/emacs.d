(require 'init-python-defs)

(defhydra python-main-mode-menu (:color pink :hint nil :exit t)
  "
^Execute ...^              ^Shell^
----------------------------------------------------
_ee_: paragraph	           _s_: shell control ...
_ek_: until cursor
_ej_: from cursor
_el_: statement
_er_: region or buffer
"
  ("ee" elpy-shell-send-paragraph)
  ("ek" elpy-shell-send-until-cursor-pos)
  ("ej" elpy-shell-send-from-cursor-pos)
  ("el" elpy-shell-send-statement)
  ("er" elpy-shell-send-region-or-buffer)

  ("s" python-shell-control-menu/body)

  ("q" nil "cancel" :color blue)
  )

(evil-define-key 'normal python-mode-map (kbd ",") 'python-main-mode-menu/body)
(evil-define-key 'visual python-mode-map (kbd ",") 'python-main-mode-menu/body)
(global-set-key (kbd "C-c ,") 'python-main-mode-menu/body)

(defhydra python-shell-control-menu (:color pink :hint nil :exit t)
  "
^Operations^               ^Window^
--------------------------------------------------
_s_: start a new shell     _w_: shell buffer
_l_: clear buffer
_k_: kill
^^
^^
"
  ("s" run-python)
  ("l" python-shell-clear)
  ;; TODO: implement following functions
  ;; ("r" python-shell-clear-and-clear-env)
  ;; ("R" python-shell-clear-and-reset-env)
  ;; TODO: add `python-reverse-truth-value' to keybinding menu
  ("k" elpy-shell-kill)

  ("w" python-open-shell-buffer)

  ("q" nil "cancel" :color blue)
  )

(add-to-list 'package-archives
	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; dependency python packages: jedi flake8 importmagic autopep8
(require-package 'elpy)
;; automatically turn on elpy-mode in *future* python buffers
(elpy-enable)  ;; it does not turn on now

(setq elpy-modules
      (remove 'elpy-module-highlight-indentation elpy-modules))
(setq elpy-modules
      (remove 'elpy-module-flymake elpy-modules))

;; TODO: make it compatible to conda
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(eval-after-load 'flymake
  '(progn
     (add-to-list 'flymake-allowed-file-name-masks
		  '("\\.py\\'" flymake-pylint-init))
     ;; function `elpy-flymake-error-at-point' was defined in init-python-defs.el
     (add-hook 'post-command-hook 'elpy-flymake-error-at-point)
     ))

(add-hook 'python-mode-hook 'init-python-mode)
(defun init-python-mode ()

  (flymake-mode 1)

  (modify-syntax-entry ?_ "w")

  (require-package 'evil-indent-textobject)

  (setq-default flymake-no-changes-timeout '2)

  ;; setup major-mode interface functions
  (setq insert-for-loop 'python-insert-for-loop)
  (setq insert-print 'python-insert-print)

  (setq insert-todo-comment 'python-insert-ptyhon-todo-comment)
  (setq insert-fixme-comment 'python-insert-ptyhon-fixme-comment)
  (setq insert-xxx-comment 'python-insert-ptyhon-xxx-comment)

  (setq append-argument 'python-insert-new-arg)
  (setq insert-argument-select 'python-avy-insert-new-arg)
  ;; end major-mode interface functions
  )

(provide 'init-python)
