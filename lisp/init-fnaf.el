
(evil-define-key 'normal org-agenda-mode-map "\\" 'org-agenda)
(evil-define-key 'normal org-agenda-mode-map "r" 'org-agenda-redo)
(evil-define-key 'normal org-agenda-mode-map "e" 'org-set-effort)
(evil-define-key 'normal org-agenda-mode-map "p" 'org-priority)

(setq org-global-properties '(("Effort_ALL". "0 0:10 0:15 0:30 1:00 2:00 3:00 4:00")))

(setq org-agenda-custom-commands
      '(
	("a" "Unsched works"
	 (
	  (tags-todo "+TODO=\"TODO\""
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "Tasks to be schedule:\n")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled
									   'deadline
									   'regexp "\\[#[A-Z]\\]"))
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  ))
	("s" "Important works"
	 (
	  (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "High priority tasks\n")
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  (agenda ""
		  (
		   (org-agenda-span 3)
		   (org-agenda-files '("~/org/inbox.org"))
		   (org-agenda-overriding-header "Upcoming deadlines:\n")
		   (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		   ))
	  ))
	("d" "Normal tasks"
	 (
	  (tags-todo "+TODO=\"TODO\"+PRIORITY=\"B\""
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "Normal priority tasks:\n")
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  (agenda ""
		  (
		   (org-agenda-span 7)
		   (org-agenda-files '("~/org/inbox.org"))
		   (org-agenda-overriding-header "Weekly deadlines:\n")
		   (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		   ))
	  ))
	("q" "Low effort placeholders"
	 (
	  (tags-todo "EFFORT>\"0:0\"&EFFORT<=\"0:15\""
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "Low effort placeholders:\n")
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  ))
	("w" "Researches"
	 (
	  (tags-todo "+TODO=\"STUDY\""
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "Researches:\n")
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  ))
	("x" "Ongoing tasks"
	 (
	  (tags-todo "scheduled"
		     (
		      (org-agenda-files '("~/org/inbox.org"))
		      (org-agenda-overriding-header "Scheduled-in tasks:\n")
		      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))
		      ))
	  ))
	))

(provide 'init-fnaf)

