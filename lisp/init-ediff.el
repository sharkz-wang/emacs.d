(require 'ediff)

(defun ediff-copy-ancestor-to-C ()
  (interactive)
  (ediff-copy-diff
   ediff-current-difference
   nil 'C nil
   (ediff-get-region-contents
    ediff-current-difference
    'Ancestor
    ediff-control-buffer)))

(defun --ediff-copy-triplet-to-C (key1 key2 key3)
  (let ((buftype1 (conv-ediff-char-to-buftype key1))
	(buftype2 (conv-ediff-char-to-buftype key2))
	(buftype3 (conv-ediff-char-to-buftype key3)))
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (if buftype1
	  (ediff-get-region-contents
	   ediff-current-difference buftype1 ediff-control-buffer)
	""
	)
      (if buftype2
	  (ediff-get-region-contents
	   ediff-current-difference buftype2 ediff-control-buffer)
	""
	)
      (if buftype3
	  (ediff-get-region-contents
	   ediff-current-difference buftype3 ediff-control-buffer)
	"")
      ))))

(defun ediff-copy-triplet-to-C ()
  (interactive)
  (let ((key1 (read-char)) (key2 (read-char)) (key3 (read-char)))
    (--ediff-copy-triplet-to-C key1 key2 key3)))

(defun conv-ediff-char-to-buftype (char)
  (cond ((memq char '(?a))    'A)
	((memq char '(?b ?B)) 'B)
	((memq char '(?A))    'Ancestor)
	(t                    nil)))

(add-hook 'ediff-keymap-setup-hook
	  (lambda ()
	    (define-key ediff-mode-map (kbd "TAB")
			#'--ediff-toggle-show-merge-status)
	    (define-key ediff-mode-map (kbd "A")
			'ediff-copy-ancestor-to-C)
	    (define-key ediff-mode-map (kbd "\\")
			'ediff-copy-triplet-to-C)

	    (set-face-attribute 'ediff-current-diff-A nil
				:background "#004151")
	    (set-face-attribute 'ediff-current-diff-B nil
				:background "#004151")
	    (set-face-attribute 'ediff-current-diff-C nil
				:background "#004151")
	    (set-face-attribute 'ediff-current-diff-Ancestor nil
				:background "#004151")
	    (set-face-attribute 'ediff-fine-diff-A nil
				:background "#53201A")
	    (set-face-attribute 'ediff-fine-diff-B nil
				:background "#53201A")
	    (set-face-attribute 'ediff-fine-diff-C nil
				:background "#53201A")
	    (set-face-attribute 'ediff-fine-diff-Ancestor nil
				:background "#53201A")
	    ))

(defun --force-restore-mode-line (func &rest args)
  (interactive)

  (apply func args)

  (when (bufferp ediff-buffer-A)
      (ediff-with-current-buffer ediff-buffer-A
	(setq mode-line-format orig-mode-line-format)))
  (when (bufferp ediff-buffer-B)
      (ediff-with-current-buffer ediff-buffer-B
	(setq mode-line-format orig-mode-line-format)))
  (when (bufferp ediff-buffer-C)
      (ediff-with-current-buffer ediff-buffer-C
	(setq mode-line-format orig-mode-line-format)))
  (when (bufferp ediff-ancestor-buffer)
      (ediff-with-current-buffer ediff-ancestor-buffer
	(setq mode-line-format orig-mode-line-format)))
  )

(advice-add 'ediff-refresh-mode-lines :around
	    #'--force-restore-mode-line)

(add-hook 'ediff-before-setup-hook
	  (lambda ()
	    (setq orig-mode-line-format mode-line-format)
	    ))

(provide 'init-ediff)
