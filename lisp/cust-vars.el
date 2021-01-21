(custom-set-variables
 '(kernel-src-repo-list
   '("~/src/linux"))
 '(org-directory "~/org")
 '(org-agenda-dir (expand-file-name "~/org"))
 '(org-snapshot-dir (expand-file-name "~/org/snapshots"))
 '(org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
 '(plantuml-jar-path (expand-file-name "~/plantuml.jar"))
 '(pdf-info-epdfinfo-program "~/epdfinfo")
 '(ycmd-server-command (list "python" (expand-file-name "~/ycmd/ycmd")))
 '(ycmd-global-config "~/ycmd/cpp/ycm/.ycm_extra_conf.py")
 )

(provide 'cust-vars)
