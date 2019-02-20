(defun buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.
   A binary buffer is defined as containing at least on null byte.
   Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current bufferis binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (let ((file-ext (file-name-extension (buffer-name))))
    (when
      (and
       (buffer-binary-p)
       (not (string= file-ext "pdf"))
       (not (string= file-ext "png"))
       (not (string= file-ext "jpg"))
       (not (string= file-ext "jpeg"))
	   )
      (hexl-mode)))))

(add-hook 'find-file-hooks 'hexl-if-binary)

(provide 'init-hex-file)
