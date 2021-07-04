;; sorted according to thickness: light -> regular -> bold -> black
;;     e.g., (set-global-frame-font "Source Code Pro:bold")
(set-global-frame-font "Ubuntu Mono")
;; (set-global-frame-font "Source Code Pro")
;; (set-global-frame-font "Monaco")
;; (set-global-frame-font "Roboto")
;; (set-global-frame-font "Press Start 2P")
(set-face-attribute 'default nil :height 200)

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
 )

;; external tools
(custom-set-variables
    '(pdf-info-epdfinfo-program "~/epdfinfo")
    '(plantuml-jar-path (expand-file-name "~/plantuml.jar"))
    '(org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
 )

(provide 'cust-vars)
