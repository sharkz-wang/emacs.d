(require 'init-magit-defs)

(defun --copy-file-location ()
  (interactive)
  (kill-new (format "%s" (buffer-file-name) (line-number-at-pos))))

;; TODO: programatically get tag name
(defun --elixir-bootlin-get-url (rev rel-path line)
  (interactive)
  (format "https://elixir.bootlin.com/linux/v5.4.81/source/%s#L%s"
          rel-path
          line))

(defun --copy-elixir-bootlin-url ()
  (interactive)
  (let ((ret-list (git-get-file-location-info)))
    (kill-new (--elixir-bootlin-get-url (nth 0 ret-list)
                                        (nth 2 ret-list)
                                        (nth 3 ret-list))
    )
  )
)

(provide 'init-external-share-menu-defs)
