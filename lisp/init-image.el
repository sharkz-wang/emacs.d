(evil-define-key 'normal image-mode-map (kbd "0") 'image-transform-reset)
(evil-define-key 'normal image-mode-map (kbd "=") 'image-increase-size)
(evil-define-key 'normal image-mode-map (kbd "+") 'image-increase-size)
(evil-define-key 'normal image-mode-map (kbd "-") 'image-decrease-size)
(evil-define-key 'normal image-mode-map (kbd "w") 'image-transform-fit-to-width)
(evil-define-key 'normal image-mode-map (kbd "W") 'image-transform-fit-to-width)
(evil-define-key 'normal image-mode-map (kbd "H") 'image-transform-fit-to-height)

(evil-define-key 'normal image-mode-map (kbd "g g") 'image-bob)
(evil-define-key 'normal image-mode-map (kbd "G") 'image-eob)

(evil-define-key 'normal image-mode-map (kbd "j")
  (lambda () (interactive) (image-next-line 1)))
(evil-define-key 'normal image-mode-map (kbd "k")
  (lambda () (interactive) (image-previous-line 1)))

(add-hook 'image-mode-hook
	  (lambda()
	    (image-transform-fit-to-height)))

(provide 'init-image)
