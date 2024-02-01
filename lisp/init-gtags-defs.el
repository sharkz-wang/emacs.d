(defun helm-gtags-dwim-new-horizontal-split () (interactive)
		   (split-window-below)
		   (other-window 1)
		   (helm-gtags-dwim)
		   (recenter)
		   (other-window 1)
		   (recenter))

(defun helm-gtags-dwim-new-vertical-split () (interactive)
		   (split-window-right)
		   (other-window 1)
		   (helm-gtags-dwim)
		   (recenter)
		   (other-window 1)
		   (recenter))

(defun delete-gtags-tags ()
  (interactive)
  ;; code ported from `helm-gtags-clear-cache'
  (let* ((tag-location (or helm-gtags--real-tag-location
			   helm-gtags--tag-location))
	 (gtags-path (concat tag-location "GTAGS"))
	 (grtags-path (concat tag-location "GRTAGS"))
	 (gpath-path (concat tag-location "GPATH")))
    (delete-file gtags-path)
    (delete-file grtags-path)
    (delete-file gpath-path)
    )
  )

(defun rebuild-gtags-tags ()
  (interactive)
  (let* ((tag-location (or helm-gtags--real-tag-location
			   helm-gtags--tag-location))
	 (tagroot tag-location)
	 (label "default")
	 (default-directory tagroot)
	 (label-opt (helm-gtags--label-option label)))
    (delete-gtags-tags)
    ;; code ported from `helm-gtags--find-tag-simple'
    (message "gtags is generating tags....")
    (if (zerop (process-file "gtags" nil nil nil "-q" label-opt))
      (message "gtags is generating tags.... done")
      (error "Failed: 'gtags -q %s'" label-opt)))
  )

(provide 'init-gtags-defs)
