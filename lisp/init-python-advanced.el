(require 'init-python-advanced-defs)

;; Notes
;;;; It seemed there's still no method to display inline plots in elpy

;; TODO: matplotlib support
;; TODO: output of numpy/pandas's data struct output were too long

(defhydra python-shell-control-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Operations^               ^Window^
--------------------------------------------------
_s_: start a new shell     _w_: shell buffer
_l_: clear buffer
_r_: restart
_k_: kill
^^
^^
"
  ("s" run-python)
  ("l" python-shell-clear)
  ("r" ipython-shell-restart)
  ;; TODO: add `python-reverse-truth-value' to keybinding menu
  ("k" elpy-shell-kill)

  ("w" python-open-shell-buffer)

  ("q" nil "cancel" :color blue)
  )

(evil-define-key 'normal python-mode-map (kbd ",") 'python-main-mode-menu/body)
(evil-define-key 'visual python-mode-map (kbd ",") 'python-main-mode-menu/body)
(global-set-key (kbd "C-c ,") 'python-main-mode-menu/body)

(defhydra python-shell-control-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Operations^               ^Window^
--------------------------------------------------
_s_: start a new shell     _w_: shell buffer
_l_: clear buffer
_r_: restart
_k_: kill
^^
^^
"
  ("s" run-python)
  ("l" python-shell-clear)
  ("r" ipython-shell-restart)
  ;; TODO: add `python-reverse-truth-value' to keybinding menu
  ("k" elpy-shell-kill)

  ("w" python-open-shell-buffer)

  ("q" nil "cancel" :color blue)
  )

;; dependency python packages: jedi flake8 importmagic autopep8
(require-package 'elpy)

;; (when (executable-find python-shell-interpreter)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; automatically turn on elpy-mode in *future* python buffers
(elpy-enable)  ;; it does not turn on now

(setq elpy-modules
      (remove 'elpy-module-highlight-indentation elpy-modules))
(setq elpy-modules
      (remove 'elpy-module-flymake elpy-modules))

(eval-after-load 'flymake
  '(progn
     (add-to-list 'flymake-allowed-file-name-masks
		  '("\\.py\\'" flymake-pylint-init))
     ;; function `elpy-flymake-error-at-point' was defined in
     ;; init-python-defs.el
     (add-hook 'post-command-hook 'elpy-flymake-error-at-point)
     ))

(provide 'init-python-advanced)
