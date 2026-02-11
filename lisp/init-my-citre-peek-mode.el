
(defvar my-citre-peek-mode-map (make-sparse-keymap)
  "Keymap for `my-citre-peek-mode'.")

(require-package 'citre)

;;;###autoload
(define-minor-mode my-citre-peek-mode
  "A minor mode so that my key settings override annoying major modes."
  :lighter " my-citre-peek-mode"
  :keymap my-citre-peek-mode-map)

;; the keymaps in `emulation-mode-map-alists' take precedence over `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-citre-peek-mode . ,my-citre-peek-mode-map)))

(add-hook 'citre-peek--mode-hook
	  (lambda () (interactive) (my-citre-peek-mode t)))
(advice-add 'citre-peek-abort :after
	#'(lambda (&rest args) (interactive) (my-citre-peek-mode -1)))

(advice-add 'citre-peek-jump :after
	#'(lambda (&rest args) (interactive) (citre-peek-abort)))

;; make evil jump list remember position before we jump
(advice-add 'citre-peek-jump :before
	#'(lambda (&rest args) (interactive) (push-current-mark)))

(evil-global-set-key 'normal (kbd "SPC d o j") 'citre-jump)
(evil-global-set-key 'normal (kbd "SPC d o r") 'citre-jump-to-reference)
(evil-global-set-key 'normal (kbd "SPC d o p") 'citre-peek)
(evil-global-set-key 'normal (kbd "SPC d o P") 'citre-peek-restore)

(define-key my-citre-peek-mode-map (kbd "j") 'citre-peek-next-line)
(define-key my-citre-peek-mode-map (kbd "k") 'citre-peek-prev-line)
(define-key my-citre-peek-mode-map (kbd "C-j") 'citre-peek-next-tag)
(define-key my-citre-peek-mode-map (kbd "C-k") 'citre-peek-prev-tag)

(define-key my-citre-peek-mode-map (kbd "RET") 'citre-peek-jump)
(define-key my-citre-peek-mode-map (kbd "p") 'citre-peek-through)
(define-key my-citre-peek-mode-map (kbd "r") 'citre-peek-through-reference)

(define-key my-citre-peek-mode-map (kbd "q") 'citre-peek-abort)

(provide 'init-my-citre-peek-mode)
