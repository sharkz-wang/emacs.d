(require-package 'hydra)

(require 'init-teleport)
(require 'init-projectile)

(defhydra hydra-file-menu (:color pink :hint nil :exit t :idle 0.3)
  "
^Save...^            ^Browse...^                ^Search...^
^^^^^^^^--------------------------------------------------------------
_s_: current buffer  _f_: current dir          _d_: current dir
^^                   _D_: dired current dir    _/_: teleport
^^                   _r_: recent files
^^                   _b_: bookmarks
^^                   _m_: teleport
^^                   _o_: in terminal
^^                   _O_: in terminal in other window
^^                   _g_: in terminal tig
"
  ;; save ...
  ("s" save-buffer)
  ;; browse ...
  ("f" helm-find-files)
  ("y" yank-buffer-file-name)
  ("d" dired-curr-dir)
  ("r" helm-recentf)
  ("b" helm-bookmarks)
  ("m" (teleport-invoke 'helm-find-files-in-dir))
  ("o" (start-process "new-proc" "proc-buffer" "tmux" "new-window" "-a"))
  ("O" (start-process "new-proc" "proc-buffer" "tmux" "split-window" "-l" "60%"))
  ("g" (start-process "new-proc" "proc-buffer" "tmux" "new-window" "-a" "tig"))
  ;; search ...
  ("D" search-file-in-current-directory)
  ("/" (teleport-invoke 'search-file-in-directory))

  ("c" nil "cancel" :color blue)
  )

;; it's annoying that unmounted directory get removed
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

(defun yank-buffer-file-name ()
  (interactive)
  (kill-new buffer-file-name)
  (message "copy file name `%s'." buffer-file-name))

(defun search-file-in-current-directory ()
    (interactive)
    (let ((default-directory (f-dirname (buffer-file-name))))
      (call-interactively 'helm-find)
      ))

(defun search-file-in-directory (dir)
    (interactive)
    (let ((default-directory dir))
      (call-interactively 'helm-find)
      ))

;; I want bookmarks to be sorted in LIFO
(setq bookmark-sort-flag nil)

(setq ffip-use-rust-fd t)
(setq ffip-rust-fd-extra-opts "--no-ignore --hidden")
(setq ffip-match-path-instead-of-filename t)

(defun my-find-file-in-project-prompt ()
  (interactive)
  (let ((fn (read-string "File name regex: ")))
    (ffip-find-files fn nil)
    ))

(defun my-find-file-in-current-directory-prompt ()
  (interactive)
  (let* ((fn (read-string "File name regex: "))
	 (ffip-project-root default-directory))
    (ffip-find-files fn nil)
    ))

(defun helm-find-files-in-dir (dir)
  (interactive)
  ;; XXX: trailing slash in path matters in `helm-file-files'
  (let ((default-directory
	  (if (file-directory-p dir)
	      (file-name-as-directory dir) dir)))
    (helm-find-files nil)
  ))

(defun dired-curr-dir (arg)
  (interactive "P")
  (dired default-directory)
  )

(evil-global-set-key 'normal (kbd "SPC f") 'hydra-file-menu/body)
(evil-define-minor-mode-key 'normal 'dired-mode-map
  (kbd "SPC f") 'hydra-file-menu/body)

(provide 'init-files)
