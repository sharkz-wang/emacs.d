(require-package 'company)

;(add-hook 'after-init-hook #'global-company-mode)
(global-company-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-selection-wrap-around t)

(define-key company-active-map (kbd "TAB") 'company-select-next)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

(provide 'init-company)
