;; sorted according to thickness: light -> regular -> bold -> black
;;     e.g., (set-global-frame-font "Source Code Pro:bold")
(set-global-frame-font "Ubuntu Mono")
;; (set-global-frame-font "Source Code Pro")
;; (set-global-frame-font "Monaco")
;; (set-global-frame-font "Roboto")
;; (set-global-frame-font "Press Start 2P")
;; (set-global-frame-font "creep")
(set-face-attribute 'default nil :height 200)
(setq cwm-centered-window-width 80)

(setq ebook-dir-path "~/ebooks")
(setq linux-repo-dir-path "~/linux")

;; repo list
(custom-set-variables
    '(kernel-src-repo-list '("~/src/linux")))

;; org-mode settings
(custom-set-variables
    '(org-directory "~/org")
    '(org-agenda-dir (expand-file-name "~/org"))
    '(org-snapshot-dir (expand-file-name "~/org/snapshots"))
    '(org-agenda-files (list (expand-file-name "~/org/agenda/inbox.org")))
    '(org-refile-targets '((org-agenda-files :maxlevel . 5)))
    '(org-capture-templates
      `(
	("z" "Quick event"
	 entry
	 (file+headline ,(concat (file-name-as-directory org-agenda-dir)
				 "quick.org")
			"Inbox")
	 "* TODO %?%i"
	 :prepend t
	 )
	("t" "Todo"
	 entry
	 (file+headline ,(concat (file-name-as-directory org-agenda-dir)
				 "inbox.org")
			"Inbox")
	 "* TODO %?%i"
	 :prepend t
	 )
	("c" "Trace code note"
	 entry
	 (file+headline ,(concat (file-name-as-directory org-agenda-dir)
				 "inbox.org")
			"Inbox")
	 ,(concat "* %?\n"
		  "[file:%F::%(--org-capture-get-file-line-num)]\n"
		  "#+BEGIN_SRC %(--org-capture-get-buffer-babel-lang)\n"
		  "%(--org-capture-get-region-code-fragment)\n"
		  "#+END_SRC")
	)
	("w" "Work log"
	 ;; Note: keyword :prepend would not work on plain items
	 plain
	 (function org-find-inbox-or-marked-entry-prepend)
	 "%(org-time-stamp '(16) nil)\n%?"
	 :empty-lines 1
	 )
	("e" "Epub note"
	 plain
	 (function org-find-inbox-or-marked-entry-append)
	 "* %?\n[[epub:%(with-current-buffer (org-capture-get :original-buffer) nov-file-name)::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string nov-documents-index))::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (point)))]]\n#+BEGIN_QUOTE c\n%(with-current-buffer (org-capture-get :original-buffer) (substring (call-interactively 'org-visual-content) 0 -1))\n#+END_QUOTE"
	 :empty-lines 1
	 )
	("p" "PDF note"
	 plain (function org-find-inbox-or-marked-entry-append)
	 "* %?\n[[pdf:%(with-current-buffer (org-capture-get :original-buffer) (pdf-view-buffer-file-name))::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (pdf-view-current-page)))::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (point)))]]\n#+BEGIN_QUOTE c\n%(with-current-buffer (org-capture-get :original-buffer) (substring (call-interactively 'org-visual-content) 0 -1))\n#+END_QUOTE"
	 :empty-lines 1
	 )
	))
 )

;; external tools
(custom-set-variables
    '(pdf-info-epdfinfo-program "~/epdfinfo")
    '(plantuml-jar-path (expand-file-name "~/plantuml.jar"))
    '(org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
 )

(provide 'cust-vars)
