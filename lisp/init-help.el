(require 'help-fns+)

(defhydra hydra-help-menu (:color pink :hint nil :exit t)
  "
^Descriptions...^           ^Man pages...^           ^Info pages...^
^^^^^^--------------------------------------------------------------
_dk_: key                   _m_: man                 _ii_: info
_df_: function                                   ^^  _ie_: info/emacs
_dv_: variable                                   ^^  _iE_: info/elisp
_dm_: mode                                       ^^  _ib_: info/bash
_dF_: face
_dK_: keymap
"

  ("dk" describe-key-and-switch-to-window)
  ("df" describe-function-and-switch-to-window)
  ("dv" describe-variable-and-switch-to-window)
  ("dm" describe-mode-and-switch-to-window)
  ("dF" describe-face-and-switch-to-window)
  ("dK" describe-keymap)

  ("m" man-and-switch-to-window)

  ("ii" info)
  ("ie" info-emacs-manual)
  ("iE" helm-info-elisp)
  ("ib" helm-info-bash)

  ("c" nil "cancel" :color blue)
  )

(defun describe-key-and-switch-to-window ()
  (interactive)
  (call-interactively 'describe-key)
  (switch-to-buffer-other-window "*Help*"))

(defun describe-function-and-switch-to-window ()
  (interactive)
  (call-interactively 'describe-function)
  (switch-to-buffer-other-window "*Help*"))

(defun describe-face-and-switch-to-window ()
  (interactive)
  (call-interactively 'describe-face)
  (switch-to-buffer-other-window "*Help*"))

(defun describe-variable-and-switch-to-window ()
  (interactive)
  (call-interactively 'describe-variable)
  (switch-to-buffer-other-window "*Help*"))

(defun describe-mode-and-switch-to-window ()
  (interactive)
  (call-interactively 'describe-mode)
  (switch-to-buffer-other-window "*Help*"))

(defun man-and-switch-to-window ()
  (interactive)
  (call-interactively 'man)
  (other-window 1))

(evil-global-set-key 'normal (kbd "SPC h") 'hydra-help-menu/body)
;; additional key-binding for minibuffers
(global-set-key (kbd "C-c h d k") 'describe-key-and-switch-to-window)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC h") 'hydra-help-menu/body)

;; make quitting man-mode easier
(evil-define-key 'normal Man-mode-map "q" 'quit-window)

(provide 'init-help)
