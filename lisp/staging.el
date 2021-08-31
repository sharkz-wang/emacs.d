;; place experimental code here

(require 'centaur-tabs-defs)

(define-key evil-normal-state-map (kbd "SPC t j") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "SPC t k")  'centaur-tabs-backward)

(define-key evil-normal-state-map (kbd "<C-tab>") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "<C-S-tab>") 'centaur-tabs-backward)

;; ii goes from 0 to 9
(dotimes (ii 10)
    (define-key evil-normal-state-map
                (kbd (format "SPC t %d" ii))
                'centaur-tabs-select-visible-tab)
    (define-key evil-normal-state-map
                (kbd (format "s-%d" ii))
                'centaur-tabs-select-visible-tab)
)

(define-key evil-normal-state-map (kbd "SPC t <tab>") '--centaur-tabs-switch-to-previous-group)
(define-key evil-normal-state-map (kbd "SPC t `") '--centaur-tabs-switch-to-group-before-last-group)

;; tabs styles: shapes/colors/indicators
;;;;  2021/06/12: there seemed not a proper way to change following styles
;;;;              after loading the package, it only works by restarting emacs
(setq centaur-tabs-style "bar")
(setq centaur-tabs-height 16)
;;;; ended "static" styles
;;;; place indicators hinting selected tabs to left side
(setq centaur-tabs-set-bar 'left)

;;;; enabled icons on tabs (and not grayed-out)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons nil)

;; buttons on tabs
(setq centaur-tabs-set-close-button t)
(setq centaur-tabs-close-button "×")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "*")

(require-package 'centaur-tabs)
(centaur-tabs-mode t)

;; tab styles: displayed texts
(centaur-tabs-change-fonts "Monaco" 150)
(setq centaur-tabs-label-fixed-length 16)
;; ended tab styles

;; lock cycling to current visible tab (a.k.a., current group)
(setq centaur-tabs-cycle-scope 'tabs)

;; TODO: find a solution for drag-and-drop
;; enabled tab reordering
(centaur-tabs-enable-buffer-reordering)
;; available options were strange where you can't do drag-and-drop,
;; tabs were just reordered according the sequence you select them
(setq centaur-tabs-adjust-buffer-order t)
(setq centaur-tabs-adjust-buffer-order 'right)

;; new tab button is less useful on emacs, let's turn it off
(setq centaur-tabs-show-new-tab-button nil)

;; make tabbar/headline shares same appearance
(centaur-tabs-headline-match)

;; group tabs by projectile projects
(centaur-tabs-group-by-projectile-project)

;; ;; TODO: `centaur-tabs-hide-tab' causes specified buffers to be completely removed
;; ;;       from tabset

;; (advice-add 'centaur-tabs-hide-tab :after-until #'--centaur-tabs-is-unpinned-tab)

;; (setq --centaur-tabs-unpinned-list '())

;; (defun --centaur-tabs-toggle-tab-unpinned ()
;;   (if (member (format "%s" (current-buffer)) --centaur-tabs-unpinned-list)
;;       (setq --centaur-tabs-unpinned-list (delete (format "%s" (current-buffer)) --centaur-tabs-unpinned-list))
;;     (progn
;;       (add-to-list '--centaur-tabs-unpinned-list (format "%s" (current-buffer)))
;;       (setq centaur-tabs-hide-hash (make-hash-table :test 'equal))
;;       )
;;     ))

;; (defun --centaur-tabs-is-unpinned-tab (x)
;;   (let ((name (format "%s" x)))
;;     (or
;;      (-contains? --centaur-tabs-unpinned-list name)
;;      )))

(define-key evil-normal-state-map
            (kbd "SPC \\")
            (lambda () (interactive) (org-sidebar-tree-toggle-buffer "inbox.org")))

(defun org-sidebar-tree-toggle-buffer (buffer-or-name)
    (interactive)
    (with-current-buffer buffer-or-name (org-sidebar-tree-toggle))
    ;; FIXME: hacky trick to force redrawing screen
    (switch-to-last-buffer)
    (switch-to-last-buffer)
)

(find-file "~/org/agenda/inbox.org")
(org-sidebar-tree-toggle-buffer "inbox.org")

(require 'init-vcs)

(evil-define-minor-mode-key 'normal
    'ediff-mode-map "j" 'evil-collection-ediff-scroll-down-1)
(evil-define-minor-mode-key 'normal
    'ediff-mode-map "k" 'evil-collection-ediff-scroll-up-1)

(evil-define-minor-mode-key 'normal
    'ediff-mode-map (kbd "C-f") 'evil-collection-ediff-scroll-down)
(evil-define-minor-mode-key 'normal
    'ediff-mode-map (kbd "C-b") 'evil-collection-ediff-scroll-up)

(defun init-ediff-buffer ()
  ;; TODO: make it into single line
  (zoom-frm-out)
  (zoom-frm-out)
  (zoom-frm-out)
  (zoom-frm-out)
  (zoom-frm-out)
  (zoom-frm-out)

  ;; disable tabs, which makes screen crowded
  (centaur-tabs-mode -1)
  ;; FIXME: hacky trick to refresh and minimize control panel
  (ediff-toggle-help)
  (ediff-toggle-help)
)
(add-hook 'ediff-startup-hook 'init-ediff-buffer)

(defun quit-ediff-buffer ()
  (zoom-frm-in)
  (zoom-frm-in)
  (zoom-frm-in)
  (zoom-frm-in)
  (zoom-frm-in)
  (zoom-frm-in)

  ;; TODO: enable it selectively
  ;; restore tabs
  (centaur-tabs-mode 1)
)
(add-hook 'magit-ediff-quit-hook 'quit-ediff-buffer)
(add-hook 'ediff-quit-hook 'quit-ediff-buffer)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))

(require-package 'gumshoe)
;; package gumshoe's dependency
(require-package 'consult)

(setq gumshoe-ignored-major-modes
      '(emacs-lisp-mode          ;; elisp buffers
        org-mode                 ;; org-mode buffers
        messages-buffer-mode     ;; *Messages*
        help-mode                ;; *Help*
        ;; XXX: don't include *scratch*, as there should always
        ;;      be at least one instance returned or `consult-global-mark'
        ;;      will list all of them anyway.
        ;;      scratch buffer was the best for this job.
        ;; lisp-interaction-mode ;; *scratch*
        ))

(defun --get-marker-buf-major-mode (marker)
    (with-current-buffer (marker-buffer marker) major-mode))

(defun --gumshoe-is-major-mode-ignored (maj-mode)
    (-any? #'identity
           (mapcar (lambda (maj) (equalp maj-mode maj))
                   gumshoe-ignored-major-modes))
)

(defun --get-marker-list-ignored-removed ()
    (seq-filter (lambda (marker)
                  (not (--gumshoe-is-major-mode-ignored
                           (--get-marker-buf-major-mode marker))))
                (ring-elements (oref gumshoe--global-backlog log)))
)

(global-gumshoe-mode 1)
(defun consult-gumshoe-global ()
    (interactive)
    (consult-global-mark (--get-marker-list-ignored-removed))
)


(evil-leader/set-key "rh" 'consult-gumshoe-global)

(evil-define-text-object evil-avy-word (count &optional beg end type)
    "Select a word, with avy."
    (save-excursion
      (evil-avy-goto-char)
      (evil-a-word count beg end type)
    )
)

(define-key evil-outer-text-objects-map "j" 'evil-avy-word)
(define-key evil-inner-text-objects-map "j" 'evil-avy-word)

(require-package 'centered-cursor-mode)

;; make magit diff buffers started with ...
;;     1. cursor moved to the center line
;;     2. buffer scrolls around the cursor
;; so we don't have to wait that little jerk crawling
;; to the last line when reading patch diffs
(defun --setup-magit ()
    (centered-cursor-mode 1)
)
(defun --advice-magit-diff-setup-buffer (RANGE TYPEARG ARGS FILES &optional LOCKED)
    ;; this statement is too early in `magit-diff-mode-hook'
    (move-to-window-line nil)
)

(advice-add 'magit-diff-setup-buffer :after #'--advice-magit-diff-setup-buffer)
(add-hook 'magit-diff-mode-hook '--setup-magit)

(provide 'staging)
