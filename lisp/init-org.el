(require-package 'org)

(defun update-org-agenda-files (dir path)
  (set dir path)
  (when (f-directory? path)
    (setq org-agenda-files (sa-find-org-file-recursively path))
    (custom-set-variables
     '(org-journal-dir
       (concat (file-name-as-directory org-agenda-dir) "journal")))))

(defcustom org-agenda-dir "~/org"
  "Default directory containing Org agenda files"
  :type 'string
  :initialize 'update-org-agenda-files
  :set 'update-org-agenda-files
  :group 'init-org)

(defcustom org-snapshot-dir "~/org/snapshots"
  "Default directory containing Org snapshot images"
  :type 'string
  :group 'init-org)

(setq org-agenda-window-setup 'only-window)

;; recursively find .org files in provided directory
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
   If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backupfiles
         (filext (or filext "org"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir)             ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))

(defun dired-snapshot-dir (arg)
  (interactive "P")
  (dired (file-name-as-directory org-snapshot-dir)))

(defun dired-fig-dir (arg)
  (interactive "P")
  (dired (concat
	  (file-name-as-directory (f-dirname (buffer-file-name)))
	  "fig")))

(evil-leader/set-key
  "ds" 'dired-snapshot-dir
  "df" 'dired-fig-dir
  )

;; org-refile settings
(setq org-refile-targets '((nil :maxlevel . 5)
			   (org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-link-parameters
      '(
	("file" :complete org-file-complete-link)
	("http" :follow (lambda (path) (browse-url (concat "http:" path))))
	("https" :follow (lambda (path) (browse-url (concat "https:" path))))
	))

(setq org-startup-folded 'nofold)
(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "WAIT(W@/!)" "|" "DONE(d!)" "ABORTED(a@)" "SUSPENDED(p@)")
	(sequence "PROJECT(P)" "|" "DONE(d!)")
	(sequence "WEEKLY(w)" "REPORT(r)" "|" "DONE(d!)")
	(sequence "STUDY(s)" "RECAP(R)" "|" "DONE(d!)")
	))
(setq org-tag-alist
      '(
	("scheduled" . ?s)
	))

(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (goto-char (point-min)))

(evil-define-operator org-visual-content (beg end type)
  :move-point nil
  :repeat nil
  (buffer-substring beg end))

(setq org-capture-templates
      '(
	("t" "Todo"
	 entry (function org-journal-find-location)
	 "* TODO %?%i\n%T")
	("c" "Trace code note"
	 entry (function org-journal-find-location)
	 "* %?\n[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos (car (evil-visual-range))())))]\n#+BEGIN_SRC c\n%(with-current-buffer (org-capture-get :original-buffer) (substring (call-interactively 'org-visual-content) 0 -1))\n#+END_SRC")
	))

(defun org-todo-list-position-to-first-heading ()
  (interactive)
  (org-todo-list)
  (switch-to-buffer "*Org Agenda*"
    (org-agenda-next-item 1))
  )

(evil-leader/set-key
  "aoo" (lambda () (interactive)
	  (find-file (format "%s/inbox.org" org-agenda-dir))
	  )
  "aoa" 'org-agenda-list
  "atl" 'org-todo-list-position-to-first-heading
  "aoc" 'org-capture
  "asp" 'helm-org-rifle-agenda-files
  "aor" 'org-refile
  )

(require-package 'helm-org-rifle)

(require-package 'org-super-agenda)

(org-super-agenda-mode 1)

(setq org-super-agenda-groups
      '(
	(:name "Scheduled"
	       :tag "scheduled"
	       :order 2
	       )
	(:name "Project"
	       :todo ("PROJECT")
	       :order 3
	       )
	(:name "Weekly goal"
	       :todo ("WEEKLY" "REPORT")
	       :order 0
	       )
	(:name "Today"
	       :deadline today
	       :scheduled today
	       :order 1
	       )
	(:name "Study"
	       :todo ("STUDY" "RECAP")
	       :order 4
	       )
	(:name "This Week"
	       :order 5
	       )
      ))

(defun init-org-handler ()
  
  (modify-syntax-entry ?- "w")

  (setq org-imenu-depth 10)

  (setq org-startup-indented 1)
  (setq org-clock-sound t)
  (setq org-timer-default-timer 25)
  (setq org-log-into-drawer t)

  (org-indent-mode 1)
  (form-feed-mode 1)

  (evil-leader/set-key
    "ati" 'org-toggle-inline-images
    "att" 'org-toggle-link-display
  )

  (evil-leader/set-key
    "ail" 'org-insert-link
    "ais" 'org-schedule
    "aid" 'org-deadline
    )
  
  (evil-define-operator org-google-search-visual (beg end type)
    :move-point nil
    :repeat nil
    (browser-google-search (buffer-substring beg end)))

  (evil-leader/set-key
    "aog" 'org-google-search-visual
  )

  (defun helm-org-rifle-current-buffer-or-occur (arg)
    (interactive "P")
    (if (equal current-prefix-arg '(4))
	(helm-org-rifle-agenda-files)
      (helm-occur)
      ))

  (evil-define-key 'normal org-mode-map
    "gk" 'org-backward-heading-same-level
    "gj" 'org-forward-heading-same-level
    "gh" 'outline-up-heading
    "gl" 'org-next-visible-heading
    "gn" 'org-next-link
    "gp" 'org-previous-link
    (kbd "TAB") 'org-cycle
    (kbd "RET") 'org-open-at-point
    "<" 'org-metaleft
    ">" 'org-metaright
    "t" 'org-todo
    (kbd "SPC s s") 'helm-org-rifle-current-buffer-or-occur
    (kbd "SPC s p") 'helm-org-rifle-agenda-files
    (kbd "SPC s f") 'helm-org-rifle-directories
    (kbd "SPC s d") (lambda () (interactive) (helm-org-rifle-directories (f-dirname (buffer-file-name))))
    )
  
(defun advice-delete-other-windows (&rest args)
  (delete-other-windows))
(advice-add 'org-open-at-point :after #'advice-delete-other-windows)

  (evil-define-operator org-open-at-point-visaul (beg end type)
  :move-point nil
  :repeat nil
    (cond
     ((eq type 'line)
      (mapcar 'org-open-link-from-string (split-string (buffer-substring beg end)))
      )))

  (evil-define-key 'visual org-mode-map
    (kbd "RET") 'org-open-at-point-visaul)

  ;; append same level heading right after current heading
  (define-key org-mode-map
    (kbd "M-RET") (lambda (arg) (interactive "P")
		    (org-insert-heading-after-current)
		    (end-of-line)
		    (evil-insert-state)
		    (when (equal current-prefix-arg '(4))
		      (org-move-subtree-up))
		    ))
  
  (add-hook 'org-capture-mode-hook
	    (lambda () (evil-emacs-state)))
  
  (require-package 'org-pdfview)

  (setq org-file-apps
	'(("\\.gif\\'" . (lambda (file link)
			   (let ((my-image (create-image file))
				 (tmpbuf (get-buffer-create "*gif-preview*")))
			     (switch-to-buffer tmpbuf)
			     (erase-buffer)
			     (insert-image my-image)
			     (image-animate my-image))))
	  ("\\.org\\'" . (lambda (file link)
			    (find-file file)
			    ))
	  ("\\.pdf\\'" . (lambda (file link)
			   (org-pdfview-open link)))
	  (auto-mode . emacs)
	  )
	)

  (custom-set-faces
   '(org-todo ((t :foreground "#FF1493" :weight bold))))
  )

(add-hook 'org-mode-hook 'init-org-handler)

(add-hook 'org-agenda-mode-hook
	  (lambda () (interactive)
	    (evil-define-key 'normal org-agenda-mode-map (kbd "RET")
	      (lambda ()
		(interactive)
		(let ((buffer (buffer-name)))
		  (org-agenda-switch-to)
		  (bury-buffer buffer))))
	    (define-key org-super-agenda-header-map "j" 'evil-next-visual-line)
	    (define-key org-super-agenda-header-map "k" 'evil-previous-visual-line)
	    (evil-define-key 'normal org-agenda-mode-map "t" 'org-agenda-todo)
	    (evil-define-key 'normal org-agenda-mode-map "s" 'helm-occur)
	    (evil-define-key 'normal org-agenda-mode-map "/" 'helm-occur)
	    (evil-define-key 'normal org-agenda-mode-map (kbd "C-c C-c") 'org-agenda-set-tags)
	    (evil-define-key 'normal org-agenda-mode-map "q" 'org-agenda-quit)
	    ))

(require-package 'org-journal)
(custom-set-variables
 '(org-journal-dir
   (concat (file-name-as-directory org-agenda-dir) "journal")))
(customize-set-variable 'org-journal-file-format "%Y%m%d.org")

(defun org-journal-create-new-entry-and-edit ()
  (interactive)
  (let ((buf (get-buffer (file-name-nondirectory (org-journal-get-entry-path)))))
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (delete-other-windows)
	  )
      (progn
	(org-journal-new-entry t)
	(delete-other-windows)
	(outline-show-all)
	(beginning-of-buffer)))))

(defun quit-all-org-journal-window ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer (buffer-name buf)
      (when (eq major-mode 'org-journal-mode)
	(bury-buffer (buffer-name buf)))))
  (switch-to-buffer (car (buffer-list))))

(defun kill-all-org-journal-buffer ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer (buffer-name buf)
      (when (eq major-mode 'org-journal-mode)
	(kill-buffer (buffer-name buf))))))

(defun bury-all-other-journal-and-switch-to-last-buffer ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (not (equal (current-buffer) buf))
     (with-current-buffer (buffer-name buf)
      (when (eq major-mode 'org-journal-mode)
	(message (format "bury: %s" (buffer-name buf)))
	(bury-buffer (buffer-name buf))))))
  (switch-to-last-buffer))

(evil-leader/set-key
  "\\" 'org-journal-create-new-entry-and-edit
  "ajj" 'org-journal-open-next-entry
  "ajk" 'org-journal-open-previous-entry
  )

(global-set-key (kbd "M-SPC") 'org-journal-create-new-entry-and-edit)

(evil-define-key 'normal org-journal-mode-map
  (kbd "SPC <tab>") 'bury-all-other-journal-and-switch-to-last-buffer)

(evil-define-key 'normal org-journal-mode-map
  (kbd "SPC b d") 'kill-all-org-journal-buffer)
(evil-define-key 'normal org-journal-mode-map
  (kbd "SPC w q") 'quit-all-org-journal-window)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; for Linux
;; export PKG_CONFIG_PATH
;; export LD_LIBRARY_PATH
;; with-imagemagick=yes
;; check ldd
;; (image-type-available-p 'imagemagick)
;; (setenv "PATH" (concat (getenv "PATH") ":/path/imagemagick/bin"))
;; (add-to-list 'exec-path (expand-file-name "/path/imagemagick/bin"))

;; for MacOS
;; `sudo port install emacs-app +imagemagick`

(setq org-image-actual-width '(500))

(defun helm-occur-and-jump-org-agenda ()
  (interactive)
  (org-todo-list)
   (with-current-buffer "*Org Agenda*"
     (helm-occur)
     (let ((buffer (buffer-name)))
       (org-agenda-switch-to)
       (bury-buffer buffer))
     )
 )
(evil-global-set-key 'normal (kbd "SPC s l") 'helm-occur-and-jump-org-agenda)

(provide 'init-org)
