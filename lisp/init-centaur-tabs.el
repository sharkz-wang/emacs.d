(require-package 'centaur-tabs)
(require 'init-centaur-tabs-defs)

(centaur-tabs-mode t)

;; core behavior setting
;;;; group tabs by projectile projects
(centaur-tabs-group-by-projectile-project)
;;;; lock cycling to current visible tab (a.k.a., current group)
(setq centaur-tabs-cycle-scope 'tabs)
;; TODO: find a solution for drag-and-drop
;;;; enabled tab reordering
(centaur-tabs-enable-buffer-reordering)
;; available options were strange where you can't do drag-and-drop,
;; tabs were just reordered according the sequence you select them
(setq centaur-tabs-adjust-buffer-order t)
(setq centaur-tabs-adjust-buffer-order 'right)

;; appearance settings
;; make tabbar/headline shares same appearance
(centaur-tabs-headline-match)
;; new tab button is less useful on emacs, let's turn it off
(setq centaur-tabs-show-new-tab-button nil)
;;;; buttons on tabs
(setq centaur-tabs-set-close-button t)
(setq centaur-tabs-close-button "×")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "*")
;; tabs styles: shapes/colors/indicators
;;;;  2021/06/12: there seemed not a proper way to change following styles
;;;;              after loading the package, it only works by restarting emacs
(setq centaur-tabs-style "bar")
(setq centaur-tabs-height 16)
;;;; ended "static" styles
;;;; place indicators hinting selected tabs to left side
(setq centaur-tabs-set-bar 'left)
;;;; enabled icons on tabs (and not grayed-out)
(if (display-graphic-p)
    (progn
      (setq centaur-tabs-set-icons t)
      (setq centaur-tabs-plain-icons t))
  (progn
    (setq centaur-tabs-set-icons nil)))
;; tab styles: displayed texts
(centaur-tabs-change-fonts "Monaco" 150)
(setq centaur-tabs-label-fixed-length 16)
;; exclude helper buffers
(add-to-list 'centaur-tabs-excluded-prefixes "magit-diff: ")
(add-to-list 'centaur-tabs-excluded-prefixes "magit-process: ")
;; end appearance settings

(define-key evil-normal-state-map (kbd "SPC t j") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "SPC t k")  'centaur-tabs-backward)

(define-key evil-normal-state-map (kbd "SPC t n") 'centaur-tabs-move-current-tab-to-right)
(define-key evil-normal-state-map (kbd "SPC t p")  'centaur-tabs-move-current-tab-to-left)

(define-key evil-normal-state-map (kbd "<C-tab>") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "<C-S-tab>") 'centaur-tabs-backward)

(define-key evil-normal-state-map (kbd "SPC t TAB") '--centaur-tabs-switch-to-previous-group)
(define-key evil-normal-state-map (kbd "SPC t `") '--centaur-tabs-switch-to-group-before-last-group)

;; ii goes from 0 to 9
(dotimes (ii 10)
    (define-key evil-normal-state-map (kbd (format "SPC t %d" ii))
		'centaur-tabs-select-visible-tab)
    (define-key evil-normal-state-map (kbd (format "s-%d" ii))
                'centaur-tabs-select-visible-tab)
)

(provide 'init-centaur-tabs)
