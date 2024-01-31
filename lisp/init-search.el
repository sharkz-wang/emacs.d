(require 'init-bookmarked-repos)

(require-package 'magit)

(defhydra hydra-search-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Buffers...^                        ^Files...^                  ^Semantics...^
^^^^^------------------------------------------------------------------------
_s_: current buffer                 _f_: browse                 _o_: outline
_S_: current buffer at point        _d_: current dir            _O_: all buffers outline
_b_: all buffers                    _m_: bookmarked dir         _j_: dumb-jump
^^                                  _/_: current project
"
  ;; buffers
  ("s" occur-dwim)
  ("S" (with-mini-buffer-input--word-at-point #'helm-occur))
  ("b" helm-do-ag-buffers)
  ("B" (with-mini-buffer-input--word-at-point #'helm-do-ag-buffers))
  ;; files
  ("f" helm-do-ag)
  ("d" helm-do-ag-curr-dir)
  ("D" (with-mini-buffer-input--word-at-point #'helm-do-ag-curr-dir))
  ("m" (hydra-bookmarked-repo-menu-action 'helm-do-ag-dir-or-file))
  ("/" helm-projectile-ag)
  ("?" (with-mini-buffer-input--word-at-point #'helm-projectile-ag))
  ;; semantics
  ("o" helm-semantic-or-imenu)
  ("O" helm-imenu-in-all-buffers-no-default)
  ("j" dumb-jump-go)

  ("c" nil "cancel" :color blue)
  )

(defun with-mini-buffer-input--word-at-point (func)
  (with-mini-buffer-input (or (word-at-point t) "") func)
  )

(defun with-mini-buffer-input (default-string func)
  (minibuffer-with-setup-hook
      (lambda () (insert (format "%s" default-string)))
    (call-interactively func))
  )

(defun helm-do-ag-dir-or-file (path)
  (interactive)
  (if (file-directory-p path)
      (helm-do-ag path)
      (helm-do-ag (file-name-directory path) (list (file-name-nondirectory path)))
    ))

(defun helm-do-ag-curr-dir ()
  (interactive)
  (helm-do-ag default-directory)
  )

(defun occur-dwim ()
  (interactive)
  (helm-occur)
)

(require-package 'dumb-jump)
(dumb-jump-mode)
(setq dumb-jump-selector 'helm)

(evil-global-set-key 'normal (kbd "SPC s") 'hydra-search-menu/body)
(evil-define-key 'normal magit-diff-mode-map (kbd "SPC s") 'hydra-search-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC s") 'hydra-search-menu/body)

(provide 'init-search)
