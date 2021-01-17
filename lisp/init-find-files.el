(require-package 'find-file-in-project)

(setq ffip-use-rust-fd t)
(setq ffip-rust-fd-extra-opts "--no-ignore --hidden")
(setq ffip-match-path-instead-of-filename t)

(defun find-file-in-project-prompt ()
  (interactive)
  (let ((fn (read-string "File name regex: ")))
    (ffip-find-files fn nil)
    ))

(defun find-file-in-current-directory-prompt ()
  (interactive)
  (let* ((fn (read-string "File name regex: "))
	 (ffip-project-root default-directory))
    (ffip-find-files fn nil)
    ))

(provide 'init-find-files)
