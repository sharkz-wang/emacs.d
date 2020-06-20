(require-package 'company)

;; (global-company-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-selection-wrap-around t)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  ;; prevent <RET> from inserting newlines when in company popup window
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  )

(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'yasnippet-snippets)

(defun exand-yasnippet-from-keyword (keyword)
  (interactive)
  (indent-according-to-mode)
  (evil-emacs-state)
  (yas-expand-snippet (yas-lookup-snippet keyword))
  )

(require-package 'helm-c-yasnippet)

(defhydra hydra-insert-menu (:color pink :hint nil :exit t)
  "
^Insert^
^^^^^^^^-------------------------
_y_: yasnippet
"
  ("y" helm-yas-complete)

  ("c" nil "cancel" :color blue)
  )

(global-set-key (kbd "C-c i") 'hydra-insert-menu/body)
(evil-global-set-key 'normal (kbd "SPC i") 'hydra-insert-menu/body)

(advice-add 'helm-yas-complete :after #'evil-insert-state)

(provide 'init-company)
