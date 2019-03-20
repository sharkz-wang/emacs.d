(require 'dired)
(require 'dired-x)
(require 'dired+)

(setq dired-dwim-target t)

(defun dired-directory-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'dired-directory-sort)

(setq dired-omit-files "^\\...+$")
(setq dired-listing-switches "-alh")

(defun dired-find-name-in-current-directory ()
  (interactive)
  (find-name-dired default-directory
                   (format "*%s*" (read-from-minibuffer "Pattern: ")))
  (set-buffer-multibyte t))

(defun dired-sort-name ()
  "Dired sort by name."
  (interactive) (dired-sort-other (concat dired-listing-switches "")))
(defun dired-sort-size ()
  "Dired sort by size."
  (interactive) (dired-sort-other (concat dired-listing-switches "S")))
(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive) (dired-sort-other (concat dired-listing-switches "X")))
(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ct")))
(defun dired-sort-utime ()
  "Dired sort by access time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ut")))
(defun dired-sort-time ()
  "Dired sort by time."
  (interactive) (dired-sort-other (concat dired-listing-switches "t")))

(evil-define-key 'normal dired-mode-map "sn" 'dired-sort-name)
(evil-define-key 'normal dired-mode-map "ss" 'dired-sort-size)
(evil-define-key 'normal dired-mode-map "se" 'dired-sort-extension)
(evil-define-key 'normal dired-mode-map "sc" 'dired-sort-ctime)
(evil-define-key 'normal dired-mode-map "su" 'dired-sort-utime)
(evil-define-key 'normal dired-mode-map "st" 'dired-sort-time)

(evil-define-key 'normal dired-mode-map "f" 'dired-find-name-in-current-directory)

(evil-define-key 'normal dired-mode-map "/" 'helm-occur)
(evil-define-key 'normal dired-mode-map "h" 'diredp-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'quit-window)

(evil-define-key 'normal dired-mode-map (kbd "SPC w -")
  (lambda () (interactive) (split-window-below) (other-window 1)))
(evil-define-key 'normal dired-mode-map (kbd "SPC w /")
  (lambda () (interactive) (split-window-right) (other-window 1)))

(defun dired-current-dir (arg)
  (interactive "P")
  (dired (f-dirname (buffer-file-name)))
  )

(defun helm-dired-recentf-dirs ()
  (interactive)
  (dired (f-dirname
	  (helm :sources
		(helm-build-sync-source "recentf directories"
		  :candidates recentf-list)))))

(defun helm-dired-projectile-dirs ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (dired (helm-projectile-project-dirs project-root))))

(defun helm-dired-projectile-project-dirs ()
  (interactive)
  (let ((project-root (helm-projectile-projects)))
    (dired (helm-projectile-project-dirs project-root))))

(evil-global-set-key 'normal (kbd "SPC d d") 'dired-current-dir)
(evil-global-set-key 'normal (kbd "SPC d p") 'helm-dired-projectile-dirs)
(evil-global-set-key 'normal (kbd "SPC d P") 'helm-dired-projectile-project-dirs)
(evil-global-set-key 'normal (kbd "SPC d r") 'helm-dired-recentf-dirs)

(evil-define-key 'normal dired-mode-map (kbd "SPC d p") 'helm-dired-projectile-dirs)
(evil-define-key 'normal dired-mode-map (kbd "SPC d P") 'helm-dired-projectile-project-dirs)
(evil-define-key 'normal dired-mode-map (kbd "SPC d r") 'helm-dired-recentf-dirs)

(evil-define-key 'normal dired-mode-map "v" 'evil-visual-char)
(evil-define-key 'normal dired-mode-map "V" 'evil-visual-line)
(evil-define-key 'normal dired-mode-map "m" 'evil-set-marker-local-global)
(evil-define-key 'normal dired-mode-map "M" 'dired-mark)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (evil-snipe-mode -1)
	    (dired-omit-mode)
	    (dired-hide-details-mode -1)
	    ))

(provide 'init-dired)
