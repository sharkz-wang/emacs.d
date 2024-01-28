
(defun --centaur-tabs-switch-to-previous-group ()
    (interactive)
    (centaur-tabs-switch-group
    (file-name-nondirectory (directory-file-name (nth 1 (projectile-open-projects))))))

(defun --centaur-tabs-switch-to-group-before-last-group ()
    (interactive)
    (centaur-tabs-switch-group
    (file-name-nondirectory (directory-file-name (nth 2 (projectile-open-projects))))))

(defun --number-in-recent-opened-buffer (buf)
    (length (member buf (buffer-list)))
)

(defun --get-recent-centaur-tabs-groups ()
    (mapcar 'symbol-name
            (mapcar 'cdr
                    (sort
                        (centaur-tabs-map-tabsets 'centaur-tabs-selected-tab)
                        (lambda (pair1 pair2)
                            (>
                                (--number-in-recent-opened-buffer (car pair1))
                                (--number-in-recent-opened-buffer (car pair2)))
                        )
                    )
             )
    )
)

(defun --nth-recent-centaur-tabs-groups (n)
    (nth n (--get-recent-centaur-tabs-groups)))

(defun --centaur-tabs-switch-to-previous-group ()
    (interactive)
    (centaur-tabs-switch-group (--nth-recent-centaur-tabs-groups 1)))

(defun --centaur-tabs-switch-to-group-before-last-group ()
    (interactive)
    (centaur-tabs-switch-group (--nth-recent-centaur-tabs-groups 2)))

(provide 'init-centaur-tabs-defs)
