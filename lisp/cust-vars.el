(set-global-frame-font "Monaco-17")

(setq linux-repo-dir-path "~/linux")

;; repo list
(custom-set-variables
    '(kernel-src-repo-list '("~/src/linux")))

;; org-mode settings
(custom-set-variables
    '(org-directory "~/org")
    '(org-agenda-dir (expand-file-name "~/org"))
    '(org-snapshot-dir (expand-file-name "~/org/snapshots"))
 )

;; external tools
(custom-set-variables
    '(pdf-info-epdfinfo-program "~/epdfinfo")
    '(plantuml-jar-path (expand-file-name "~/plantuml.jar"))
    '(org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
 )

(provide 'cust-vars)
