(require-package 'org)

;; recursively find .org files in provided directory
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
   If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backupfiles
         (filext (or filext "org$\\\|org_archive"))
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

(setq org-agenda-files '("~/org"))

(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "WAIT(W@/!)" "|" "DONE(d!)" "ABORTED(a@)" "SUSPENDED(p@)")
	(sequence "WEEKLY(w)" "|" "REPORT(r)")
	(sequence "STUDY(s)" "|" "RECAP(R)")
	))
(setq org-tag-alist
      '(
	("scheduled" . ?s)
	))

(setq org-capture-templates
      '(
	("t" "Todo"
	 entry (file+headline "~/note-system.org" "Tasks")
	 "* TODO %?%i\t%^g\n%T")
	("c" "Trace code note"
	 entry (file+olp "~/gtd.org" "Trace Code")
	 "* %?%i\t%^g\n%T\n[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]\n%c")
	)
      )

(evil-leader/set-key
  "aoa" 'org-agenda-list
  "atl" 'org-todo-list
  "aoc" 'org-capture
  "asp" 'helm-org-rifle-agenda-files
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
	(:name "Weekly goal"
	       :todo "WEEKLY"
	       :order 0
	       )
	(:name "Today"
	       :deadline today
	       :scheduled today
	       :order 1
	       )
	(:name "Study"
	       :todo "STUDY"
	       :order 3
	       )
	(:name "This Week"
	       :order 4
	       )
      ))

(defun init-org-handler ()
  
  (setq org-startup-indented 1)
  (setq org-clock-sound t)
  (setq org-timer-default-timer 25)
  (setq org-log-into-drawer t)

  (evil-leader/set-key
    "ati" 'org-toggle-inline-images
    "att" 'org-toggle-link-display
  )

  (evil-leader/set-key
    "ail" 'org-insert-link
    "ais" 'org-schedule
    "aid" 'org-deadline
    )
  
  (evil-define-key 'normal org-mode-map
    "gk" 'org-backward-heading-same-level
    "gj" 'org-forward-heading-same-level
    "gh" 'outline-up-heading
    "gl" 'org-next-visible-heading
    (kbd "TAB") 'org-cycle
    (kbd "RET") 'org-open-at-point
    "<" 'org-metaleft
    ">" 'org-metaright
    "t" 'org-todo
    (kbd "SPC s s") 'helm-org-rifle-current-buffer
    (kbd "SPC s p") 'helm-org-rifle-agenda-files
    (kbd "SPC s f") 'helm-org-rifle-directories
    (kbd "SPC s d") (lambda () (interactive) (helm-org-rifle-directories (f-dirname (buffer-file-name))))
    )
  
  ;; append same level heading right after current heading
  (define-key org-mode-map
    (kbd "M-RET") (lambda (arg) (interactive "P")
		    (org-insert-heading-after-current)
		    (end-of-line)
		    (evil-insert-state)
		    (when (equal current-prefix-arg '(4))
		      (org-move-subtree-up))
		    ))
  
  (evil-define-key 'normal org-agenda-mode-map "q" 'org-agenda-quit)
  (define-key org-super-agenda-header-map "j" 'evil-next-visual-line)
  (define-key org-super-agenda-header-map "k" 'evil-previous-visual-line)

  (add-hook 'org-capture-mode-hook
	    (lambda () (evil-emacs-state)))
  
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
			    )))
	)

  (custom-set-faces
   '(org-todo ((t :foreground "#FF1493" :weight bold))))
  )

(add-hook 'org-mode-hook 'init-org-handler)

(provide 'init-org)
