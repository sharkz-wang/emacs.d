(require 'init-org-capture-defs)
;; use faster github mirror instead of savannah one
(straight-register-package '(org :type git :repo "https://github.com/bzg/org-mode" :depth 1))
(require-package 'org)

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

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-link-parameters
      '(
	("file" :complete org-file-complete-link)
	("http" :follow (lambda (path) (browse-url (concat "http:" path))))
	("https" :follow (lambda (path) (browse-url (concat "https:" path))))
	))

(setq org-startup-folded 'content)
(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "WAIT(W@/!)" "|" "DONE(d!)" "ABORTED(a@)" "SUSPENDED(p@)")
	(sequence "PROJECT(P)" "|" "DONE(d!)")
	(sequence "WEEKLY(w)" "REPORT(r)" "|" "DONE(d!)")
	(sequence "STUDY(s)" "RECAP(R)" "|" "DONE(d!)")
	))
(setq org-tag-alist
      '(
	("today" . ?t)
	("scheduled" . ?s)
	("livingroom" . ?l)
	))

(evil-define-operator org-visual-content (beg end type)
  :move-point nil
  :repeat nil
  (buffer-substring beg end))

;; Looking for evil marker `?o' first when doing org-capture.
;; Otherwise, insert into inbox.org as first entry.
(defun org-find-inbox-or-marked-entry-append ()
  (interactive)
  (if (get-global-mark ?o)
    (progn
      (evil-goto-global-mark-line ?o)
	   (org-next-visible-heading 1)
	   (previous-line 1)
      )
    (progn
      (find-file (format "%s/inbox.org" org-agenda-dir))
      (beginning-of-buffer)
      )
  ))

;; Looking for evil marker `?i' first when doing org-capture.
;; Otherwise, insert into inbox.org as first entry.
(defun org-find-inbox-or-marked-entry-prepend ()
  (interactive)
  (if (get-global-mark ?i)
    (progn
      (evil-goto-global-mark-line ?i)
	   (next-line 1)
      )
    (progn
      (find-file (format "%s/inbox.org" org-agenda-dir))
      (beginning-of-buffer)
      )
  ))

(defun org-todo-list-position-to-first-heading ()
  (interactive)
  (org-todo-list)
  (switch-to-buffer "*Org Agenda*"
    (org-agenda-next-item 1))
  )

(defun org-todo-list-position-to-first-heading-or-refresh ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
	(switch-to-buffer "*Org Agenda*")
	(org-agenda-redo)
	)
    (progn
      (org-todo-list-position-to-first-heading)
      )))

(global-set-key (kbd "C-\\") 'org-todo-list-position-to-first-heading-or-refresh)

(defun org-capture-force-horizontal ()
  (interactive)
  (cl-letf ((split-width-threshold nil)
	    (split-height-threshold 10))
    (org-capture)))

(require-package 'org-sticky-header)

(setq org-sticky-header-full-path 'full)
(setq org-sticky-header-heading-star " ")
(setq org-sticky-header-always-show-header t)
(setq org-sticky-header-outline-path-separator " > ")

(require-package 'helm-org-rifle)

(require 'init-fnaf)
(defun init-org-handler ()
  
  (modify-syntax-entry ?- "w")
  (load-theme 'green-black-org-mode t)

  (setq org-imenu-depth 10)

  (setq org-hide-emphasis-markers t)

  (setq org-startup-indented 1)
  (setq org-clock-sound t)
  (setq org-timer-default-timer 25)
  (setq org-log-into-drawer t)
  (setq org-use-tag-inheritance t)

  (setq org-src-tab-acts-natively t)

  ;; reusing level-n style is good enough
  (setq org-cycle-level-faces nil)

  (require-package 'org-superstar)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-superstar-mode 1)

	      (push '("[ ]" . "☐") prettify-symbols-alist)
	      (push '("[X]" . "✓") prettify-symbols-alist)
	      (push '("[-]" . "☒") prettify-symbols-alist)
	      (prettify-symbols-mode 1)

	      (org-sticky-header-mode)
	      ))

  (setq org-superstar-headline-bullets-list
	'("" "○"  "·" "·" "·" "·" "·" "·"))
  ;; reusing last entry in bullet list is good enough
  (setq org-superstar-cycle-headline-bullets nil)

  ;; space between headlines and underlines,
  ;; used to make level-1 headlines less strange with underlines
  (setq underline-minimum-offset 7)

  (setq org-emphasis-alist
	'(("*" bold)
	  ("/" italic)
	  ("_" underline)
	  ("~" org-verbatim verbatim)
	  ("=" (:background "blue"))
	  ("+" (:strike-through t))))

  (setq org-startup-indented t
	org-ellipsis " ▾ " ;; folding symbol
	org-hide-emphasis-markers t
	;; show actually italicized text instead of /italicized text/
	org-agenda-block-separator ""
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t)

  (org-indent-mode 1)
  (form-feed-mode 1)

  (global-hl-line-mode 1)
  (setq line-spacing 0.1)

  (evil-leader/set-key
    "ati" 'org-toggle-inline-images
    "att" 'org-toggle-link-display
  )

  (defun org-insert-heading-respect-content-and-edit ()
    (interactive)
    (org-insert-heading-respect-content)
    (evil-emacs-state)
    )

  (defun org-insert-todo-heading-respect-content-and-edit ()
    (interactive)
    (org-insert-todo-heading-respect-content)
    (evil-emacs-state)
    )

(defun org-move-to-top ()
  "Move current org subtree to the end of its parent.
   With prefix arg move subtree to the start of its parent."
  (interactive "P")
  (condition-case err
      (while t
        (funcall 'org-move-subtree-up))
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg)))))))

  (defun org-insert-subheading-respect-content-and-edit ()
      (interactive)
      (org-insert-heading-respect-content-and-edit)
      (org-shiftmetaright)
      (org-move-to-top)
      (move-end-of-line nil)
      )

  (defun org-insert-todo-subheading-respect-content-and-edit ()
      (interactive)
      (org-insert-todo-heading-respect-content-and-edit)
      (org-shiftmetaright)
      )

(defun insert-image-from-screenshot-dir ()
  (interactive)
  (let ((src-file
	 (concat "~/Desktop/" (helm :sources
	       (helm-build-sync-source "screenshot directory"
		 :candidates
		 (mapcar #'car (sort (directory-files-and-attributes "~/Desktop/")
				     #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))))
		 )
	(dest-dir
	 (file-name-as-directory (helm :sources
	       (helm-build-sync-source "target directory"
		 :candidates (directory-files "."))))))
    (rename-file src-file (concat dest-dir (file-name-nondirectory src-file)) t)
    (org-insert-link nil (concat "file:" (concat dest-dir (file-name-nondirectory src-file))))
    (org-display-inline-images)
    )
  )

  (defun org-copy-block ()
    (interactive)
    (org-narrow-to-block)
    (beginning-of-buffer)
    (next-line)
    (evil-beginning-of-line)
    (evil-visual-line)
    (end-of-buffer)
    (previous-line)
    (evil-end-of-line)
    (kill-ring-save (region-beginning) (region-end))
    (widen))

  (evil-leader/set-key
    "ail" 'org-insert-link
    "aiy" 'org-store-link
    "aic" 'org-copy-block
    "aiL" 'insert-image-from-screenshot-dir
    "ait" 'org-time-stamp
    "ais" 'org-schedule
    "aid" 'org-deadline
    "aii" 'org-insert-heading-respect-content-and-edit
    "aiI" 'org-insert-subheading-respect-content-and-edit
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

  (defun org-back-to-heading-or-backward-heading-same-level (arg)
    (interactive "P")
	(org-backward-heading-same-level
	 (if (org-at-heading-p) 1 0))
      )

  (evil-define-key 'normal org-mode-map
    "q"  'quit-window
    "gk" 'org-back-to-heading-or-backward-heading-same-level
    "gj" 'org-forward-heading-same-level
    "gh" 'outline-up-heading
    "gl" 'org-next-visible-heading
    "gG" (lambda ()
	   (interactive)
	   (org-next-visible-heading 1)
	   (previous-line 1)
	   )
    "gn" 'org-next-link
    "gp" 'org-previous-link
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "TAB") 'org-cycle
    (kbd "RET") 'org-open-at-point
    (kbd "M-h") 'org-shiftmetaleft
    (kbd "M-l") 'org-shiftmetaright
    "<" 'org-metaleft
    ">" 'org-metaright
    "t" 'org-todo
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
  
  (setq org-file-apps
	'(("\\.org\\'" . (lambda (file link) (find-file file)))
	  (auto-mode . emacs)))
  )

(eval-after-load 'org
  '(progn (init-org-handler)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (when (display-graphic-p)
		(text-scale-decrease 1))
	    (company-mode -1)))

(add-hook 'org-agenda-mode-hook
	  (lambda () (interactive)
	    (evil-define-key 'normal org-agenda-mode-map (kbd "RET")
	      (lambda ()
		(interactive)
		(let ((buffer (buffer-name)))
		  (org-agenda-switch-to)
		  (bury-buffer buffer))))
	    (evil-local-set-key 'normal "p" 'org-agenda-priority)
	    (evil-local-set-key 'normal "u" 'org-agenda-undo)
	    (evil-local-set-key 'normal "o"
				(lambda ()
				  (interactive)
				  (evil-define-key 'normal org-mode-map
				    "q"
				    (lambda ()
				      (interactive)
				      (quit-window)
				      (evil-define-key 'normal org-mode-map "q" 'evil-record-macro))
				    )
				  (let ((split-height-threshold 80)
					(split-width-threshold 160))
				    (org-agenda-goto)
				    )
				  ))
	    (evil-define-key 'normal org-agenda-mode-map "t" 'org-agenda-todo)
	    (evil-define-key 'normal org-agenda-mode-map "s" 'helm-occur)
	    (evil-define-key 'normal org-agenda-mode-map "/" 'helm-occur)
	    (evil-define-key 'normal org-agenda-mode-map (kbd "C-c C-c") 'org-agenda-set-tags)
	    (evil-define-key 'normal org-agenda-mode-map "q" 'org-agenda-quit)
	    ))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; add plantuml support
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

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
;(evil-global-set-key 'normal (kbd "SPC s l") 'helm-occur-and-jump-org-agenda)
;(evil-global-set-key 'normal (kbd "SPC s I") 'helm-org-in-buffer-headings)

(require-package 'org-sidebar)

(defun org-sidebar-tree-toggle-buffer (buffer-or-name)
  (interactive)
  (find-file-noselect (format "%s/inbox.org" org-agenda-dir))
  (with-current-buffer "inbox.org"
    (org-sidebar-tree-toggle))
  (select-window (car (window-at-side-list
		       (selected-frame) 'left)))
  (bury-buffer (get-buffer "<tree>inbox.org"))
)

(require 'init-org-capture)

(provide 'init-org)
