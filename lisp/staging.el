;; place experimental code here

;; TODO: programatically get tag name
(defun elixir-bootlin-get-url (rev rel-path line)
  (interactive)
  (format "https://elixir.bootlin.com/linux/v4.9.92/source/%s#L%s"
	  rel-path
	  line))

(provide 'staging)
