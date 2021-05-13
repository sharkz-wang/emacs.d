(require-package 'company)

;; we use inexpensive backend by default,
;; so activate company-mode in all situations
(global-company-mode)

;; TODO: parameterize delay/prefix-len when adding more expensive backends
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

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

  ;; like alt+0-9, but now we also allow a more CTS-friendly \+0-9
  (dotimes (ii 10)
    (define-key company-active-map
      (kbd (format "\\ %d" ii)) 'company-complete-number))
  )

(require 'company-dabbrev-code)
(custom-set-variables
 ;; don't turn expansion into lowercase
 '(company-dabbrev-downcase nil)
 '(company-backends '(company-dabbrev-code)))

;; TODO: vim-ycmd-like subsequence matching
;; TODO: should `dabbrev-case-replace' be here?
;; (not company) don't change expansion's original case when `dabbrev-expand'ing
(setq dabbrev-case-replace nil)
;; don't do case-sensitive matching in company
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)

;; use `company-sort-by-occurrence'
(setq company-transformers '(company-sort-by-occurrence))
;; when use `company-sort-by-occurrence', sort according
;; candidate's relative distance to cursor's position
(setq company-occurrence-weight-function #'company-occurrence-prefer-any-closest)

(provide 'init-company)
