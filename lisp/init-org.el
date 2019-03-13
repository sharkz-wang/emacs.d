(require-package 'org)
(load-file "~/.emacs.d/elpa/org-pdfview-20180225.1006/org-pdfview.el")

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

(setq org-agenda-dir "")
(setq org-agenda-files (sa-find-org-file-recursively org-agenda-dir))

(setq org-startup-folded 'nofold)
(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "WAIT(W@/!)" "|" "DONE(d!)" "ABORTED(a@)" "SUSPENDED(p@)")
	(sequence "WEEKLY(w)" "REPORT(r)" "|" "DONE(d!)")
	(sequence "STUDY(s)" "RECAP(R)" "|" "DONE(d!)")
	))
(setq org-tag-alist
      '(
	("scheduled" . ?s)
	))

(setq org-capture-templates
      '(
	("t" "Todo"
	 entry (file+headline "~/note-system.org" "Tasks")
	 "* TODO %?%i\n%T")
	("c" "Trace code note"
	 entry (file+olp "~/gtd.org" "Trace code")
	 "* %?%i\n%T\n[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]\n#+BEGIN_SRC c\n%c\n#+END_SRC")
	)
      )

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
	       :order 3
	       )
	(:name "This Week"
	       :order 4
	       )
      ))

(defun init-org-handler ()
  
  (modify-syntax-entry ?- "w")

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

(provide 'init-org)
