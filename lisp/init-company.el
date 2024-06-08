(require-package 'company)
(require-package 'company-fuzzy)
(require 'init-company-defs)

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

  ;; make tooltip's keystroke marks aligned to left
  ;; e.g.,
  ;;     'a company
  ;;     's company-active-map
  ;;     ^^
  (setq company-show-numbers 'left)

  ;; use homerow keystrokes for tooltip completion
  (init-company-completion-keystrokes
      ;; homerow keystrokes
      '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)
      ;; we use a more CTS-friendly '+n instead of meta key
      "'%c")
  )

(require 'company-dabbrev-code)
(custom-set-variables
 ;; don't turn expansion into lowercase
 '(company-dabbrev-downcase nil)
 ;; memo: when dinamically update it, remember to check
 ;;       `company-safe-backends-p'
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
(setq company-occurrence-weight-function #'company-occurrence-prefer-closest-above)

;; company-fuzzy part
;; note: company-flx does not support dabbrev now
(global-company-fuzzy-mode 1)

;; let's not enable it so it won't look like a mess
;; e.g.,
;;         company-fuzzy-show-annotation          <dabbrev-code> 1
;;                                                ^^^^^^^^^^^^^^
;; but it's useful for debugging though
(setq company-fuzzy-show-annotation nil)

;; though we don't usually enable tooltip annotations,
;; but let;s align it to right if we ever do it
(setq company-tooltip-align-annotations t)

;; TODO: move it to where it should be
(custom-set-faces
 ;; whole company tooltip box
 '(company-tooltip ((t (:background "#3C3D37"))))
 ;; highlighted candidate row
 '(company-tooltip-selection
   ((t (:background "#64645E" :foreground "#FFFFFF"))))
 ;; company-fuzzy's matched characters
 '(company-tooltip-common
   ((t (:background "#3C3D37" :foreground "#66D9EF" :underline nil))))
 ;; company-fuzzy's matched characters, selected
 '(company-tooltip-common-selection
   ((t (:background "#64645E" :foreground "#66D9EF" :underline nil))))
 ;; scrollbar's rolling part
 '(company-scrollbar-fg ((t (:background "#64645E"))))
 '(company-scrollbar-bg ((t (:background "#3C3D37"))))
   )

(provide 'init-company)
