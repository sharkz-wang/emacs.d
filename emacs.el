;; -*- lexical-binding: t; -*-

(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set recipes
 (setq
 el-get-sources
 '(
   (:name el-get)

   ;; Vim-emulation
   (:name evil-indent-textobject)
   (:name evil-numbers)
   (:name evil-leader)
   (:name evil-nerd-commenter)
   (:name evil-org-mode
	  :type git
	  :url "https://github.com/edwtjo/evil-org-mode")
   (:name evil-visual-mark-mode
	  :type git
	  :url "https://github.com/roman/evil-visual-mark-mode")

   (:name evil-jumper)

   ;; (:name golden-ratio)
   (:name transpose-frame)

   (:name adaptive-wrap)
   (:name ace-window
	  :type git
	  :url "https://github.com/abo-abo/ace-window")

   (:name undo-tree)
   (:name paredit)
   (:name highlight-parentheses)

   (:name avy
		:type git
		:url "https://github.com/abo-abo/avy")
   (:name history
		:type git
		:url "https://github.com/boyw165/history")
   (:name smooth-scroll)

   (:name smex				; a better (ido like) M-x
	  :after (progn
		(setq smex-save-file "~/.emacs.d/.smex-items")
		(add-hook 'ido-setup-hook
		    (lambda ()
			(define-key ido-completion-map (kbd "C-h") 'delete-backward-char)

			(define-key ido-completion-map (kbd "C-j") 'ido-next-match)
			(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
			(define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
			(define-key ido-completion-map (kbd "C-p") 'ido-prev-match)

			(define-key ido-completion-map (kbd "C-? f") 'smex-describe-function)
			(define-key ido-completion-map (kbd "C-? w") 'smex-where-is)))

			(global-set-key (kbd "M-x") 'smex)
			(global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name evil-magit
		:type git
		:url "https://github.com/justbur/evil-magit")

   (:name with-editor)

   (:name magit				; git meet emacs, and a binding
	  :after (progn
		   (setq magit-log-margin (quote (t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

		   ;; set magit popup windows default to full-screen
		   (setq magit-display-buffer-function
			 #'magit-display-buffer-fullframe-status-v1)

		   ;; Unbind SPC - leading key of many useful key-bindings
		   (eval-after-load 'magit '(define-key magit-mode-map (kbd "SPC") nil))
		   (eval-after-load 'magit '(define-key magit-diff-mode-map (kbd "SPC") nil))
		   (eval-after-load 'magit '(define-key magit-blame-mode-map (kbd "SPC") nil))

		   (define-key evil-normal-state-map (kbd "SPC m 1 TAB") 'magit-section-show-level-1-all)
		   (global-set-key (kbd "C-c m 1 TAB") 'magit-section-show-level-1-all)
		   (define-key evil-normal-state-map (kbd "SPC m 2 TAB") 'magit-section-show-level-2-all)
		   (global-set-key (kbd "C-c m 2 TAB") 'magit-section-show-level-2-all)
		   (define-key evil-normal-state-map (kbd "SPC m 3 TAB") 'magit-section-show-level-3-all)
		   (global-set-key (kbd "C-c m 3 TAB") 'magit-section-show-level-3-all)
		   (define-key evil-normal-state-map (kbd "SPC m 4 TAB") 'magit-section-show-level-4-all)
		   (global-set-key (kbd "C-c m 4 TAB") 'magit-section-show-level-4-all)

		   (define-key evil-normal-state-map (kbd "SPC m e") 'magit-ediff-stage)

		   (define-key evil-normal-state-map (kbd "SPC m m") 'magit-status)
		   (define-key evil-normal-state-map (kbd "SPC TAB") 'magit-status)
		   (global-set-key (kbd "C-c m s") 'magit-status)
		   (define-key evil-normal-state-map (kbd "SPC m s") 'magit-status)
		   (global-set-key (kbd "C-c m $") 'magit-process-buffer)
		   (define-key evil-normal-state-map (kbd "SPC m $") 'magit-process-buffer)
		   (global-set-key (kbd "C-c m 4") 'magit-process-buffer)
		   (define-key evil-normal-state-map (kbd "SPC m 4") 'magit-process-buffer)
		   (global-set-key (kbd "C-c m B") 'magit-show-refs-popup)
		   (define-key evil-normal-state-map (kbd "SPC m B") 'magit-show-refs-popup)
		   (defun selectively-show-magit-diff (arg) (interactive "P")
						     (if (equal current-prefix-arg '(4))
							   (magit-diff-staged)
							   (magit-diff-unstaged)
						       )
						     )
		   (global-set-key (kbd "C-c m d") 'selectively-show-magit-diff)
		   (define-key evil-normal-state-map (kbd "SPC m d") 'selectively-show-magit-diff)
		   (global-set-key (kbd "C-c m f d") 'magit-diff-buffer-file)
		   (define-key evil-normal-state-map (kbd "SPC m f d") 'magit-diff-buffer-file)
		   (global-set-key (kbd "C-c m f b d") 'magit-ediff-compare)
		   (define-key evil-normal-state-map (kbd "SPC m f b d") 'magit-ediff-compare)
		   (global-set-key (kbd "C-c m D") 'magit-diff)
		   (define-key evil-normal-state-map (kbd "SPC m D") 'magit-diff)
		   (global-set-key (kbd "C-c m f l") 'magit-log-buffer-file)
		   (define-key evil-normal-state-map (kbd "SPC m f l") 'magit-log-buffer-file)
		   (global-set-key (kbd "C-c m b") 'magit-blame)
		   (define-key evil-normal-state-map (kbd "SPC m b") 'magit-blame)
		   (global-set-key (kbd "C-c m c") 'magit-commit)
		   (define-key evil-normal-state-map (kbd "SPC m c") 'magit-commit)
		   (global-set-key (kbd "C-c m r") 'magit-rebase-interactive)
		   (define-key evil-normal-state-map (kbd "SPC m r") 'magit-rebase-interactive)
		   (global-set-key (kbd "C-c m a") 'magit-commit-amend)
		   (define-key evil-normal-state-map (kbd "SPC m a") 'magit-commit-amend)
		   (global-set-key (kbd "C-c m i") 'magit-rebase-continue)
		   (define-key evil-normal-state-map (kbd "SPC m i") 'magit-rebase-continue)
		   (global-set-key (kbd "C-c m R") 'magit-ediff-resolve)
		   (define-key evil-normal-state-map (kbd "SPC m R") 'magit-ediff-resolve)
		   (global-set-key (kbd "C-c m p") 'magit-push)
		   (define-key evil-normal-state-map (kbd "SPC m p") 'magit-push)
		   (global-set-key (kbd "C-c m P") 'magit-pull)
		   (define-key evil-normal-state-map (kbd "SPC m P") 'magit-pull)
		   (global-set-key (kbd "C-c m l") 'magit-log-current)
		   (define-key evil-normal-state-map (kbd "SPC m l") 'magit-log-current)
		   (global-set-key (kbd "C-c m L") 'magit-log)
		   (define-key evil-normal-state-map (kbd "SPC m L") 'magit-log)
		   (global-set-key (kbd "C-c m C") 'magit-clean)
		   (define-key evil-normal-state-map (kbd "SPC m C") 'magit-clean)
		   (global-set-key (kbd "C-c m o") 'magit-checkout)
		   (define-key evil-normal-state-map (kbd "SPC m o") 'magit-checkout)
		   (global-set-key (kbd "C-c m z") 'magit-stash)
		   (define-key evil-normal-state-map (kbd "SPC m z") 'magit-stash)
		   (global-set-key (kbd "C-c m w") 'magit-diff-toggle-refine-hunk)))

   (:name emacs-git-gutter
	  :type git
	  :url "https://github.com/syohex/emacs-git-gutter")
   (:name ztree)

   (:name deferred)
   (:name popup)

   (:name company-mode)
   (:name company-statistics)
   (:name emacs-ycmd
	  :type git
	  :url "https://github.com/abingham/emacs-ycmd")
   (:name cedet)
   (:name ecb)
   (:name yasnippet)

   (:name elpy)

   (:name helm-dash)

   (:name emacs-w3m)

   (:name semantic-refactor
	  :type git
	  :url "https://github.com/tuhdo/semantic-refactor")

   (:name function-args)

   (:name ggtags)
   (:name helm
	  ;; hides `.' in file list (hiding both `.' and `..`'
	  ;; invalidate helm-find-files-up-one-level)
	  :after (progn
		   (advice-add 'helm-ff-filter-candidate-one-by-one
			       :around (lambda (fcn file)
					 (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1\\}\\'" file)
					   (funcall fcn file))))
		   ))
   (:name helm-ag)
   (:name helm-gtags)

   (:name f)
   (:name projectile)
   (:name helm-projectile)

   (:name uncrustify-mode
		:type git
		:url "https://github.com/koko1000ban/emacs-uncrustify-mode")

   (:name linum-relative)

   (:name sr-speedbar)
   (:name dtrt-indent)

   ;(:name ess)

   (:name auctex
		:type git
		:url "https://github.com/jwiegley/auctex")
   (:name langtool)

   (:name cperl-mode)

   (:name clojure-mode)
   (:name cider)
   (:name ac-cider
		:type git
		:url "https://github.com/clojure-emacs/ac-cider")

   (:name org-mode
	  :after (progn
		   (global-set-key (kbd "C-c t") 'org-todo)))

   (:name markdown-mode)

   (:name indent-guide)

   (:name Fill-Column-Indicator
      :type git
      :url "https://github.com/alpaker/Fill-Column-Indicator")

   (:name hl-spotlight
      :type elpa)

   ))

;; install new packages and init already installed packages
(el-get 'sync (loop for src in el-get-sources collect (el-get-source-name src)))

(setq inhibit-splash-screen t)		; no splash screen, thanks
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

(set-default-font "Deja Vu Sans Mono-16")
;(set-face-attribute 'default nil :height 140)

(tool-bar-mode -1)			; no tool bar with icons
;(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)

(global-hl-line-mode)			; highlight current line

;; (global-linum-mode 1)			; add line numbers on the left
(setq left-margin-width 3)

(defun toggle-linum-mode ()
  (interactive)
  (if (bound-and-true-p linum-mode)
      (progn
	(linum-mode 0)
	(setq left-margin-width 3))
    (linum-mode 1))
  )

(setq linum-format 'linum-relative)

(require 'linum-relative)
(setq linum-relative-current-symbol "")
(custom-set-faces
  '(linum-relative-current-face ((t :inherit hl-spotlight :foreground "#FF8700"))))

(defadvice linum-update (around hl-linum-update)
		     (let ((linum-current-line-number (line-number-at-pos)))
			       ad-do-it))
(ad-activate 'linum-update)

(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (if (eq linum-format 'linum-format-func)
		(setq-local linum-format-fmt
			    (let ((w (length (number-to-string
					      (count-lines (point-min) (point-max))))))
			      (concat " %" (number-to-string w) "d   ")))
	      (setq linum-relative-format
		    (let ((w (length (number-to-string
				      (count-lines (point-min) (point-max))))))
		      (concat " %" (number-to-string w) "s   "))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face
			   (if (eq linum-current-line-number line)
			   '((t :inherit hl-spotlight :foreground "#FF8700"))
			   '((t :inherit linum))))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)

;; (setq browse-url-browser-function (quote browse-url-firefox))
(setq browse-url-browser-function 'w3m-goto-url-new-session)

(define-key global-map (kbd "C-w") 'evil-delete-backward-word)
(define-key global-map (kbd "C-x C-o") 'ff-find-other-file)

(setq-default indent-tabs-mode t)
(setq c-default-style "k&r")
(setq-default tab-width 8)
;(defvaralias 'c-basic-offset 'tab-width)

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

(setq windmove-wrap-around t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

(global-set-key (kbd "C-c r") (lambda () (interactive) (load-file "~/.emacs")))

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC f o") 'helm-find-files)

(global-set-key (kbd "C-c C") 'calendar)

(define-key evil-normal-state-map (kbd "SPC i l") 'toggle-linum-mode)

(global-set-key (kbd "C-c i e") (lambda () (interactive) (find-file "~/.emacs")))
(define-key evil-normal-state-map (kbd "SPC i e") (lambda () (interactive) (find-file "~/.emacs")))

(define-key evil-normal-state-map (kbd "SPC x c") 'save-buffers-kill-terminal)
(define-key evil-normal-state-map (kbd "SPC x q") 'save-buffers-kill-terminal)

(add-hook 'org-mode-hook (lambda ()
			   (require 'ob-ditaa)
			   (require 'ob-dot)
			   ))
(setq org-ditaa-jar-path "/opt/ditaa/ditaa.jar")

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(setq org-agenda-files '("~/gtd.org"))
(global-set-key (kbd "C-c o o") (lambda () (interactive) (find-file "~/gtd.org")))
(define-key evil-normal-state-map (kbd "SPC o o")
  (lambda () (interactive) (find-file "~/gtd.org")))

(global-set-key (kbd "C-x C") 'calc-dispatch)

(global-set-key (kbd "M-z") 'smex)
(define-key evil-normal-state-map (kbd "SPC z") 'smex)

(global-set-key (kbd "C-c i k") 'describe-key)
(define-key evil-normal-state-map (kbd "SPC i k") 'describe-key)
(global-set-key (kbd "C-c i f") 'describe-function)
(define-key evil-normal-state-map (kbd "SPC i f") 'describe-function)

(evil-global-set-key 'normal (kbd "z m") 'evil-scroll-line-to-center)
(evil-global-set-key 'normal (kbd "z z") (lambda () (interactive)
					   (evil-scroll-line-to-center (line-number-at-pos))
					   (evil-scroll-line-down (/ (window-total-height) 5))))

(evil-global-set-key 'normal (kbd "C-f") (lambda () (interactive)
					   (evil-scroll-page-down 1)
					   (evil-window-middle)))
(evil-global-set-key 'normal (kbd "C-b") (lambda () (interactive)
					   (evil-scroll-page-up 1)
					   (evil-window-middle)))
(evil-global-set-key 'normal (kbd "C-d") (lambda () (interactive)
					   (evil-window-bottom)
					   (recenter)))
(evil-global-set-key 'normal (kbd "C-M-u") (lambda () (interactive)
					   (evil-window-top)
					   (recenter)))

(global-set-key (kbd "C-c o a") 'org-agenda)
(define-key evil-normal-state-map (kbd "SPC o a") 'org-agenda)
(global-set-key (kbd "C-c o t") 'org-todo-list)
(define-key evil-normal-state-map (kbd "SPC o t") 'org-todo-list)
(global-set-key (kbd "C-c o l") 'org-agenda-list)
(define-key evil-normal-state-map (kbd "SPC o l") 'org-agenda-list)
(global-set-key (kbd "C-c o T") 'org-set-tags)
(define-key evil-normal-state-map (kbd "SPC o T") 'org-set-tags)
(global-set-key (kbd "C-c o c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC o c") 'org-capture)
(global-set-key (kbd "C-c o C") 'org-columns)
(define-key evil-normal-state-map (kbd "SPC o C") 'org-columns)
(global-set-key (kbd "C-c o p") 'org-set-property)
(define-key evil-normal-state-map (kbd "SPC o p") 'org-set-property)

(define-key evil-normal-state-map  (kbd "SPC o O") 'org-open-at-point)

(defalias 'outline-show-all 'show-all)

(setq org-capture-templates '(("t" "Todo" entry (file+headline "~/gtd.org" "Tasks")
			       "* TODO %?%i\t%^g\n%T")
			      ("c" "Trace code note" entry (file+olp "~/gtd.org" "Trace Code")
			       "* %?%i\t%^g\n%T\n[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]\n%c")))

(define-key evil-normal-state-map (kbd "SPC s") 'save-buffer)

(global-set-key (kbd "C-x C-x") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC x x") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC \'") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "SPC \;") 'ido-switch-buffer)

(define-key evil-normal-state-map (kbd "SPC i w") 'toggle-truncate-lines)

;; Toggling paste state - something like vim's `set paste' mode
(define-key evil-normal-state-map (kbd "C-c i p") '(lambda () (interactive)
						     (if (bound-and-true-p company-mode)
							 (progn
							   (company-mode -1)
							   (message "Ready to paste."))
							 (progn
							   (company-mode t)
							   (message "Stop paste state.")))
						     ))
(define-key evil-normal-state-map (kbd "SPC i p") '(lambda () (interactive)
						     (if (bound-and-true-p company-mode)
							 (progn
							   (company-mode -1)
							   (message "Ready to paste."))
							 (progn
							   (company-mode t)
							   (message "Stop paste state.")))
						     ))
(define-key evil-normal-state-map (kbd "SPC e l") 'eval-last-sexp)
(define-key evil-normal-state-map (kbd "SPC e e") 'eval-last-sexp)

(defun switch-to-last-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x l") 'switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "SPC l") (lambda () (interactive)
						  (flop-frame)
						  (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC j") 'switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "SPC x l") 'switch-to-last-buffer)

(define-key evil-normal-state-map (kbd "SPC x c") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-below) (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC x 2") (lambda () (interactive) (split-window-below) (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC 2") (lambda () (interactive) (split-window-below) (other-window 1)))

(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-right) (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC x 3") (lambda () (interactive) (split-window-right) (other-window 1)))
(define-key evil-normal-state-map (kbd "SPC 3") (lambda () (interactive) (split-window-right) (other-window 1)))

(define-key evil-normal-state-map (kbd "SPC 1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC 0") 'delete-window)

(define-key evil-normal-state-map (kbd "SPC 4") 'evil-end-of-visual-line)
(define-key evil-visual-state-map (kbd "SPC 4") 'evil-end-of-visual-line)
(define-key evil-normal-state-map (kbd "SPC 5") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "SPC b g ") 'end-of-buffer)

(global-set-key (kbd "C-q") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC q") 'delete-other-windows)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "SPC x b") 'helm-buffers-list)

(global-set-key (kbd "C-x B") 'ibuffer)
(define-key evil-normal-state-map (kbd "SPC B") 'ibuffer)

(global-set-key (kbd "C-x m") 'evil-visual-mark-mode)
(define-key evil-normal-state-map (kbd "SPC x m") 'evil-visual-mark-mode)

(define-key evil-normal-state-map (kbd "SPC x k b") 'ido-kill-buffer)

(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd "SPC x k k") 'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd "SPC K") 'kill-buffer-and-window)

(global-set-key (kbd "C-x C-d") 'ediff-buffers)
(define-key evil-normal-state-map (kbd "SPC x d") 'ediff-buffers)

(global-set-key (kbd "C-x D") 'ediff-files)
(define-key evil-normal-state-map (kbd "SPC f d f") 'ediff-files)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defvar global-keybinding-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-q") 'kill-buffer-and-window)
    map)
  "global-keybinding-minor-mode keymap.")

(define-minor-mode global-keybinding-minor-mode
  "A minor mode for overriding all mode-specific key-bindings"
  :init-value t
  :lighter "global-keybinding")

(global-keybinding-minor-mode 1)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(require 'package)
; marmalade repo
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/") t)
; MELPA repo
(add-to-list 'package-archives
	'("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'evil)
(evil-mode t)

(evil-jumper-mode t)
(evil-visual-mark-mode t)

(require 'evil-org)

(add-hook 'evil-org-mode-hook
	  (lambda ()
	    (evil-define-key 'normal evil-org-mode-map (kbd "TAB") 'org-cycle)
	    (evil-define-key 'normal evil-org-mode-map (kbd "M-<") 'org-metaleft)
	    (evil-define-key 'normal evil-org-mode-map (kbd "M->") 'org-metaright)
	    (evil-define-key 'normal evil-org-mode-map (kbd "<") 'evil-shift-left)
	    (evil-define-key 'normal evil-org-mode-map (kbd ">") 'evil-shift-right)
	    (evil-define-key 'normal evil-org-mode-map (kbd "H") 'evil-window-top)
	    (evil-define-key 'normal evil-org-mode-map (kbd "L") 'evil-window-bottom)
	    (define-key org-mode-map (kbd "M-RET") (lambda (arg) (interactive "P")
						     (org-insert-heading-after-current)
						     (end-of-line)
						     (evil-insert-state)
						     (when (equal current-prefix-arg '(4))
						       (org-move-subtree-up))
						     ))
	    ))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'org-mode-hook
	  (lambda () (modify-syntax-entry ?_ "w")
	    (add-to-list 'org-modules "org-habit")))
(add-hook 'org-capture-mode-hook
	  (lambda () (evil-emacs-state)))
(add-hook 'c-mode-hook
    (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook
    (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook
    (lambda () (modify-syntax-entry ?_ "w")
				;; (elpy-enable)
				;; (flymake-mode t)
				(define-key python-mode-map (kbd "C-c C-c") 'compile)
				(define-key python-mode-map (kbd "C-c C-d") 'elpy-doc)

				(defun python-insert-print ()
				  (interactive)
				  (insert "print(\"\")")
				  (left-char 2)
				  (evil-insert-state)
				  (indent-according-to-mode))
				(define-key python-mode-map (kbd "C-c d p") 'python-insert-print)
				(evil-define-key 'motion python-mode-map (kbd "SPC d p") 'python-insert-print)

				(defun python-insert-formated-string ()
				  (interactive)
				  (insert "\"\" % ()")
				  (left-char 6)
				  (evil-insert-state)
				  (indent-according-to-mode))
				(define-key python-mode-map (kbd "C-c d s") 'python-insert-formated-string)
				(evil-define-key 'motion python-mode-map (kbd "SPC d s") 'python-insert-formated-string)

				(defun python-append-formated-string-param ()
				  (interactive)
				  (search-forward "\"")
				  (insert " % ()")
				  (left-char)
				  (evil-insert-state)
				  (indent-according-to-mode))
				(define-key python-mode-map (kbd "C-c a s") 'python-append-formated-string-param)
				(evil-define-key 'motion python-mode-map (kbd "SPC a s") 'python-append-formated-string-param)

				(defun python-insert-formated-string-print ()
				  (interactive)
				  (insert "print(\"\" % ())")
				  (left-char 7)
				  (evil-insert-state)
				  (indent-according-to-mode))
				(define-key python-mode-map (kbd "C-c d P") 'python-insert-formated-string-print)
				(evil-define-key 'motion python-mode-map (kbd "SPC d P") 'python-insert-formated-string-print)

				(defun python-insert-exit ()
				  (interactive)
				  (insert "exit(1)")
				  (indent-according-to-mode))
				(define-key python-mode-map (kbd "C-c d e") 'python-insert-exit)
				(evil-define-key 'motion python-mode-map (kbd "SPC d e") 'python-insert-exit)

				(defun python-insert-new-arg () (interactive)
				       (search-forward ")")
				       (left-char)
				       (insert ", ")
				       (evil-insert-state))
				(define-key python-mode-map (kbd "C-c a ,") 'python-insert-new-arg)
				(evil-define-key 'motion python-mode-map (kbd "SPC a ,") 'python-insert-new-arg)

				(defun python-avy-insert-new-arg () (interactive)
				       (avy-goto-char-in-line ?,)
				       (right-char)
				       (insert " ,")
				       (left-char)
				       (evil-insert-state))
				(define-key python-mode-map (kbd "C-c i ,") 'python-avy-insert-new-arg)
				(evil-define-key 'motion python-mode-map (kbd "SPC i ,") 'python-avy-insert-new-arg)

				(defun python-reverse-truth-value () (interactive)
				       (if (string= (thing-at-point 'word) "True")
					   (progn
					     (evil-forward-word-begin 1)
					     (evil-delete-backward-word)
					     (insert "False")
					     )
					 (if (string= (thing-at-point 'word) "False")
					     (progn
					       (evil-forward-word-begin 1)
					       (evil-delete-backward-word)
					       (insert "True")
					       )
					   )
					 ))
				(define-key python-mode-map (kbd "C-c d i") 'python-reverse-truth-value)
				(evil-define-key 'motion python-mode-map (kbd "SPC d i") 'python-reverse-truth-value)

				(defun python-insert-reminder-comment (keyword) (interactive)
				       (if (current-line-empty-p)
					   (progn
					     (end-of-line)
					     (insert (format "# %s: " keyword))
					     (indent-according-to-mode)
					     (evil-insert-state)
					     )
					 (progn
					   (beginning-of-line)
					   (newline)
					   (previous-line)
					   (end-of-line)
					   (insert (format "# %s: " keyword))
					   (indent-according-to-mode)
					   (evil-insert-state)
					   )
					 ))

				(defun python-insert-ptyhon-todo-comment ()
				  (interactive)
				  (python-insert-reminder-comment "TODO"))
				(define-key python-mode-map (kbd "C-c d c t")
				  'python-insert-ptyhon-todo-comment)
				(evil-define-key 'motion python-mode-map (kbd "SPC d c t")
				  'python-insert-ptyhon-todo-comment)

				(defun python-insert-ptyhon-fixme-comment ()
				  (interactive)
				  (python-insert-reminder-comment "FIXME"))
				(define-key python-mode-map (kbd "C-c d c f")
				  'python-insert-ptyhon-fixme-comment)
				(evil-define-key 'motion python-mode-map (kbd "SPC d c f")
				  'python-insert-ptyhon-fixme-comment)

				(defun python-insert-ptyhon-xxx-comment ()
				  (interactive)
				  (python-insert-reminder-comment "XXX"))
				(define-key python-mode-map (kbd "C-c d c x")
				  'python-insert-ptyhon-xxx-comment)
				(evil-define-key 'motion python-mode-map (kbd "SPC d c x")
				  'python-insert-ptyhon-xxx-comment)
				))
(add-hook 'shellscript-mode-hook
    (lambda () (define-key shellscript-mode-map (kbd "C-c C-c") 'compile)))
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?_ "w")
	    (defun elisp-insert-formatted-string-print ()
	      (interactive)
	      (insert "(message (format \"\"))")
	      (left-char 3)
	      (evil-insert-state)
	      (indent-according-to-mode))
	    (define-key emacs-lisp-mode-map (kbd "C-c d p")
			    'elisp-insert-formatted-string-print)
	    (evil-define-key 'motion emacs-lisp-mode-map (kbd "SPC d p")
			    'elisp-insert-formatted-string-print)
	    ))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "\\")

(require 'evil-nerd-commenter)

(defun evilnc-invert-comment-line-by-line (&optional NUM) (interactive "p")
       (setq evilnc-invert-comment-line-by-line t)
       (evilnc-comment-or-uncomment-lines NUM)
       (setq evilnc-invert-comment-line-by-line nil))

(defun comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-region beg end)))

(defun uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (uncomment-region beg end)))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(evil-leader/set-key
  "cc" 'comment-region-or-line
  "cu" 'uncomment-region-or-line
  "ci" 'evilnc-invert-comment-line-by-line
  "cy" 'evilnc-copy-and-comment-lines
  "cf" '(lambda() (interactive)
	  (srecode-document-insert-comment)
	  (evil-insert-state)
	  (previous-line)
	  (previous-line)
	  (end-of-line)
	  (if (current-line-empty-p)
	      nil
	      (newline-and-indent))
	  (next-line)
	  (next-line)
	  (end-of-line)
	  )
  "\\" 'evilnc-comment-operator)

(require 'evil-org)

(define-key evil-normal-state-map (kbd "_") '(lambda () (interactive)
					       (message (buffer-file-name
							 (window-buffer (minibuffer-selected-window))))))

(define-key evil-normal-state-map (kbd "B") '(lambda () (interactive)
					       (message (substring
							 (shell-command-to-string
							  "git rev-parse --abbrev-ref HEAD")
							 0
							 -1))))

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(define-key evil-normal-state-map (kbd "C-w") 'ace-window)
(define-key evil-normal-state-map (kbd "SPC w") 'ace-window)
(define-key evil-normal-state-map (kbd "SPC x o") 'ace-window)

(define-key global-map (kbd "C-c w") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?l aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
  "List of actions for `aw-dispatch-default'.")

(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark-line)

;; (setcdr evil-insert-state-map [escape])
;; (define-key evil-insert-state-map
;; 	(read-kbd-macro evil-toggle-key) 'evil-emacs-state)
;; (define-key evil-insert-state-map [escape] 'evil-normal-state)

;; Cursor motion in wrapped lines
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "$" 'evil-end-of-visual-line)

(defadvice evil-insert-state (around emacs-state-instead-of-insert-state activate)
  (evil-emacs-state))

(define-key evil-emacs-state-map (kbd "C-r") 'evil-paste-from-register)
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map "k" #'cofi/maybe-exit-kj)
(define-key evil-emacs-state-map "j" #'cofi/maybe-exit-jk)

(evil-define-command cofi/maybe-exit-kj ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(evil-define-command cofi/maybe-exit-jk ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(require 'org)
(setq org-startup-indented 1)
(setq org-clock-sound t)
(setq org-timer-default-timer 25)

(custom-set-faces
  '(org-todo ((t :foreground "#FF1493" :weight bold))))

(defun nolinum ()
  (interactive)
  (global-linum-mode 0)
  (linum-mode 0)
)
;; (add-hook 'org-mode-hook 'nolinum)

(custom-set-faces
 `(company-tooltip-selection ((t (:foreground ,"#F5F5F5" :background ,"#444444"))))
 `(company-tooltip-common-selection ((t (:foreground ,"#F5F5F5" :background ,"#444444"))))
 `(company-tooltip-common ((t (:foreground ,"#F5F5F5" :background ,"#444444")))))

(hl-spotlight-mode 1)
(setq hl-spotlight-height 0)
(custom-set-faces
  '(hl-spotlight ((t :inherit highlight :weight bold))))

;; make scroll smooth
(setq scroll-step 1)
(setq scroll-margin 0
scroll-conservatively 0)
(setq-default scroll-up-aggressively 0.01
scroll-down-aggressively 0.01)

(define-key global-map (kbd "C-c SPC") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "SPC SPC") 'avy-goto-char-2)

(require 'compile)

(setq compilation-scroll-output t)

;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 		 (unless (file-exists-p "Makefile")
;; 			  (set (make-local-variable 'compile-command)
;; 		   ;; emulate make's .c.o implicit pattern rule, but with
;; 		   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
;; 		   ;; variables:
;; 		   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
;; 				   (let ((file (file-name-nondirectory buffer-file-name)))
;; 		     (format "%s -c -o %s.o %s %s %s"
;; 			     (or (getenv "CC") "gcc")
;; 			     (file-name-sans-extension file)
;; 			     (or (getenv "CPPFLAGS") "-DDEBUG=9")
;; 			     (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
;; 			       file))))))

(setq compile-command "make")
 (add-hook 'c-mode-hook
           (lambda ()
	          (unless (file-exists-p "Makefile")
		           (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
				    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -o %s %s %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			      file (format "%s%s" "&& ./" (file-name-sans-extension file))))))))

(require 'saveplace)
(setq-default save-place t)

(require 'undo-tree)
(global-undo-tree-mode 1)

(require 'helm-files)
(require 'helm-config)
(require 'helm-grep)

(helm-mode 1)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key evil-normal-state-map (kbd "SPC h") 'helm-command-prefix)

(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "C-c h o") 'helm-org-agenda-files-headings)

(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-l") 'helm-find-files-down-last-level)

(define-key evil-normal-state-map (kbd "SPC h j") (lambda () (interactive) (helm-resume ()) (helm-next-line)))

;; Making GNU Global support more languages
;; 1) Install Exuberant Ctags
;; 2) Run `pip install pygments`
;; 3) Copy /usr/local/share/gtags/gtags.conf to ~/.globalrc
;; 4) Update ~/.globalrc: change pigments-parser.la and exuberant-ctags.la to *.so and correct their path
;; 5) Build tags by running `gtags --gtagslabel=pygments`
(require 'helm-gtags)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(setq
    helm-gtags-ignore-case t
    helm-gtags-auto-update t
    helm-gtags-use-input-at-cursor t
    helm-gtags-pulse-at-cursor t
    helm-gtags-prefix-key "\C-c g"
    helm-gtags-suggested-key-mapping t
)

(global-set-key (kbd "C-c h i") 'helm-imenu)
(define-key evil-normal-state-map (kbd "SPC h i") 'helm-imenu)
(global-set-key (kbd "C-c h r") 'helm-resume)
(define-key evil-normal-state-map (kbd "SPC h r") 'helm-resume)
(global-set-key (kbd "C-c h b") 'helm-bookmark)
(define-key evil-normal-state-map (kbd "SPC h b") 'helm-bookmark)
(global-set-key (kbd "C-c h g") 'helm-occur)
(global-set-key (kbd "C-c g g") 'helm-occur)
(define-key evil-normal-state-map (kbd "SPC h g") 'helm-occur)
(define-key evil-normal-state-map (kbd "SPC g g") 'helm-occur)
(define-key evil-normal-state-map (kbd "SPC t") 'helm-occur)
(define-key evil-normal-state-map (kbd "SPC h a") 'helm-do-ag)

(define-key helm-gtags-mode-map (kbd "C-c g S") 'helm-gtags-select)
(define-key evil-normal-state-map (kbd "SPC g S") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c g d") 'helm-gtags-dwim)
(define-key evil-normal-state-map (kbd "SPC g d") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-find-pattern)
(define-key evil-normal-state-map (kbd "SPC g p") 'helm-gtags-find-pattern)
(define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-find-files)
(define-key evil-normal-state-map (kbd "SPC g f") 'helm-gtags-find-files)
(define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
(define-key evil-normal-state-map (kbd "SPC g r") 'helm-gtags-find-rtag)
(define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
(define-key evil-normal-state-map (kbd "SPC g s") 'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
(define-key evil-normal-state-map (kbd "SPC g t") 'helm-gtags-find-tag)

(require 'sr-speedbar)
(setq sr-speedbar-width 25)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-right-side nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)

(global-set-key (kbd "C-\\") '(lambda () (interactive)
				(if (sr-speedbar-exist-p)
				    (sr-speedbar-close)
				  (if (sr-speedbar-buffer-exist-p speedbar-buffer)
				      (sr-speedbar-open)
				    (progn (sr-speedbar-open) (evil-goto-first-line))))))
(defun ido-speedbar-buffers-to-end ()
  (let ((speedbar-buffer (delq nil (mapcar
			      (lambda (x)
				(if (string-match "SPEEDBAR" x)
				    x))
			      ido-temp-list))))
    (ido-to-end speedbar-buffer)))
(add-hook 'ido-make-buffer-list-hook 'ido-speedbar-buffers-to-end)

;; (add-hook 'emacs-startup-hook (lambda () (sr-speedbar-open) (other-window 1)))
(defun speedbar-edit-line-and-close () (interactive)
  (speedbar-edit-line)
  (sr-speedbar-close))

(setq speedbar-show-hidden-file "^$")
(setq speedbar-default-visibility speedbar-directory-unshown-regexp)
(add-hook 'speedbar-mode-hook '(lambda () ()
				 (linum-mode 0)
				 (visual-line-mode 0)
				 (setq case-fold-search t)

				 (define-key speedbar-file-key-map
				   "."
				   '(lambda () (interactive)
				      (if (eq speedbar-directory-unshown-regexp speedbar-show-hidden-file)
					  (setq  speedbar-directory-unshown-regexp speedbar-default-visibility)
					(setq speedbar-directory-unshown-regexp speedbar-show-hidden-file))
				      (speedbar-refresh)))
				 ;; (define-key speedbar-file-key-map (kbd "SPC SPC") 'avy-goto-char-2)
				 (evil-define-key 'motion speedbar-file-key-map
				   (kbd "TAB") 'speedbar-toggle-line-expansion)
				 (define-key speedbar-file-key-map "n" 'evil-search-next)
				 (define-key speedbar-file-key-map "N" 'evil-search-previous)
				 (define-key speedbar-file-key-map "q" '(lambda () (interactive)
									  (kill-buffer "*SPEEDBAR*")))
				 (evil-define-key 'motion speedbar-file-key-map (kbd "RET") 'speedbar-edit-line)
				 (define-key speedbar-file-key-map "e" 'speedbar-edit-line)
				 (define-key speedbar-file-key-map "d" 'speedbar-item-delete)
				 (define-key speedbar-file-key-map "y" 'speedbar-item-copy)
				 (define-key speedbar-file-key-map "c" 'speedbar-item-copy)
				 (define-key speedbar-file-key-map "r" 'speedbar-item-rename)
				 (define-key speedbar-file-key-map "m" 'speedbar-item-rename)
				 (define-key speedbar-key-map "g" 'evil-goto-first-line)))

(require 'semantic)
;(global-semanticdb-minor-mode 1)
;(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
;(global-semantic-idle-summary-mode 1)
(global-semantic-stickyfunc-mode 1)

(define-key semantic-mode-map (kbd "C-c g i") 'semantic-ia-show-summary)

(defun helm-gtags-dwim-new-horizontal-split () (interactive)
  (split-window-below)
  (other-window 1)
  (helm-gtags-dwim)
  (recenter)
  (other-window 1)
  (recenter))
(define-key semantic-mode-map (kbd "C-c g 2 d") 'helm-gtags-dwim-new-horizontal-split)
(define-key evil-normal-state-map (kbd "SPC g 2 d") 'helm-gtags-dwim-new-horizontal-split)

(defun helm-gtags-dwim-new-vertical-split () (interactive)
  (split-window-right)
  (other-window 1)
  (helm-gtags-dwim)
  (recenter)
  (other-window 1)
  (recenter))
(define-key semantic-mode-map (kbd "C-c g 3 d") 'helm-gtags-dwim-new-vertical-split)
(define-key evil-normal-state-map (kbd "SPC g 3 d") 'helm-gtags-dwim-new-vertical-split)

;(semantic-add-system-include "/usr/include/boost" 'c++-mode)
;(semantic-add-system-include "~/linux/kernel")
;(semantic-add-system-include "~/linux/include")

(require 'srefactor)
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

(require 'company)
(add-hook 'after-init-hook #'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-select-next)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

;(define-key company-active-map (kbd "SPC") (lambda () (interactive) (company-complete-selection) (insert " ")))
;(define-key company-active-map (kbd ".") (lambda () (interactive) (company-complete-selection) (insert ".")))
;(define-key company-active-map (kbd "-") (lambda () (interactive) (company-complete-selection) (insert "-")))
;(define-key company-active-map (kbd ")") (lambda () (interactive) (company-complete-selection) (insert ")")))
;(define-key company-active-map (kbd "}") (lambda () (interactive) (company-complete-selection) (insert "}")))
;(define-key company-active-map (kbd ">") (lambda () (interactive) (company-complete-selection) (insert ">")))
;(define-key company-active-map (kbd ";") (lambda () (interactive) (company-complete-selection) (insert ";")))

;; ycmd Installation
;; 1) git clone https://github.com/Valloric/ycmd.git
;; 2) git submodule update --init --recursive
;; 3) install mono-xbuild and mono-devel
;; 4) ./build.py --clang-completer --omnisharp-completer --gocode-completer --system-libclang

;(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(setq ycmd-force-semantic-completion t)
(set-variable 'ycmd-server-command (list "python" (expand-file-name "/opt/ycmd/ycmd")))
(set-variable 'ycmd-global-config "/opt/ycmd/cpp/ycm/.ycm_extra_conf.py")
(require 'company-ycmd)
(company-ycmd-setup)
;(require 'flycheck-ycmd)
;(flycheck-ycmd-setup)

(require 'yasnippet)
(yas-global-mode 1)

(global-set-key (kbd "M-/") 'yas-expand)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(require 'cc-mode)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun private-c-c++-mode-hook ()

  (eval-after-load 'c-mode '(define-key c-mode-map (kbd "M-j") nil))
  ;; Auto indenting and pairing curly brace
  (defun c-mode-insert-lcurly ()
	(interactive)
	(insert "{")
	(let ((pps (syntax-ppss)))
	  (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
		(c-indent-line)
		(insert "\n\n}")
		(c-indent-line)
		(forward-line -1)
		(c-indent-line))))
  (define-key c-mode-base-map "{" 'c-mode-insert-lcurly)

  (require 'uncrustify-mode)
  ;(uncrustify-mode 1)
  (setq uncrustify-config-path "~/.uncrustify/linux-kernel.cfg")

  (defun company-transform-c-c++ (candidates)
	(let ((deleted))
	  (mapcar #'(lambda (c)
				  (if (string-prefix-p "_" c)
					(progn
					  (add-to-list 'deleted c)
					  (setq candidates (delete c candidates)))))
			  candidates)
	  (append candidates (nreverse deleted))))
  (setq-local company-transformers
	      (append company-transformers
		      '(company-sort-by-occurrence
			company-transform-c-c++)))

  (defun insert-printf-stderr ()
	(interactive)
	(insert "fprintf(stderr, \"\\n\");")
	(left-char 5)
	(evil-insert-state)
	(indent-according-to-mode))
  (define-key c-mode-map (kbd "C-c d p") 'insert-printf-stderr)
  (evil-define-key 'motion c-mode-map (kbd "SPC d p") 'insert-printf-stderr)
  (define-key c++-mode-map (kbd "C-c d p") 'insert-printf-stderr)
  (evil-define-key 'motion c++-mode-map (kbd "SPC d p") 'insert-printf-stderr)

  (defun insert-exit ()
	(interactive)
	(insert "exit(1);")
	(indent-according-to-mode))
  (define-key c-mode-map (kbd "C-c d e") 'insert-exit)
  (evil-define-key 'motion c-mode-map (kbd "SPC d e") 'insert-exit)
  (define-key c++-mode-map (kbd "C-c d e") 'insert-exit)
  (evil-define-key 'motion c++-mode-map (kbd "SPC d e") 'insert-exit)

  (defun insert-new-arg () (interactive)
	(search-forward ";")
	(search-backward ")")
	(insert ", ")
	(evil-insert-state))
  (define-key c-mode-map (kbd "C-c a ,") 'insert-new-arg)
  (evil-define-key 'motion c-mode-map (kbd "SPC a ,") 'insert-new-arg)
  (define-key c++-mode-map (kbd "C-c a ,") 'insert-new-arg)
  (evil-define-key 'motion c++-mode-map (kbd "SPC a ,") 'insert-new-arg)

  (defun avy-insert-new-arg () (interactive)
	(avy-goto-char-in-line ?,)
	(right-char)
	(insert " ,")
	(left-char)
	(evil-insert-state))
  (define-key c-mode-map (kbd "C-c i ,") 'avy-insert-new-arg)
  (evil-define-key 'motion c-mode-map (kbd "SPC i ,") 'avy-insert-new-arg)
  (define-key c++-mode-map (kbd "C-c i ,") 'avy-insert-new-arg)
  (evil-define-key 'motion c++-mode-map (kbd "SPC i ,") 'avy-insert-new-arg)

  (defun insert-c-reminder-comment (keyword) (interactive)
	 (if (current-line-empty-p)
	     (progn
	       (end-of-line)
	       (insert (format "/* %s:  */" keyword))
	       (left-char 3)
	       (indent-according-to-mode)
	       (evil-insert-state)
	       )
	   (progn
	     (beginning-of-line)
	     (newline)
	     (previous-line)
	     (end-of-line)
	     (insert (format "/* %s:  */" keyword))
	     (left-char 3)
	     (indent-according-to-mode)
	     (evil-insert-state)
	     )
	   ))

  (defun insert-c-todo-comment ()
    (interactive)
    (insert-c-reminder-comment "TODO"))
  (define-key c-mode-map (kbd "C-c d c t")
    'insert-c-todo-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c t")
    'insert-c-todo-comment)
  (define-key c-mode-map (kbd "C-c d c t")
    'insert-c++-todo-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c t")
    'insert-c++-todo-comment)

  (defun insert-c-fixme-comment ()
    (interactive)
    (insert-c-reminder-comment "FIXME"))
  (define-key c-mode-map (kbd "C-c d c f")
    'insert-c-fixme-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c f")
    'insert-c-fixme-comment)
  (define-key c-mode-map (kbd "C-c d c f")
    'insert-c++-fixme-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c f")
    'insert-c++-fixme-comment)

  (defun insert-c-xxx-comment ()
    (interactive)
    (insert-c-reminder-comment "XXX"))
  (define-key c-mode-map (kbd "C-c d c x")
    'insert-c-xxx-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c x")
    'insert-c-xxx-comment)
  (define-key c-mode-map (kbd "C-c d c x")
    'insert-c++-xxx-comment)
  (evil-define-key 'motion c-mode-map (kbd "SPC d c x")
    'insert-c++-xxx-comment)
)

(add-hook 'compilation-mode-hook '(lambda ()
				    (local-unset-key "g")
				    (local-unset-key "h")
				    (evil-define-key 'motion compilation-mode-map "g" 'evil-goto-first-line)
				    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
				    (evil-define-key 'motion compilation-mode-map "r" 'recompile)
				    ))

(define-key c-mode-map (kbd "C-c C-c") 'compile)
(define-key evil-normal-state-map (kbd "SPC c c") 'compile)
(define-key c-mode-map (kbd "C-c C-k") 'mode-compile-kill)
(define-key evil-normal-state-map (kbd "SPC c k") 'mode-compile-kill)
(define-key c-mode-map (kbd "C-c C-r") 'recompile)
(define-key evil-normal-state-map (kbd "SPC c r") 'recompile)

(add-hook 'c-mode-hook 'private-c-c++-mode-hook)

(define-key c++-mode-map (kbd "C-c C-c") 'compile)
(define-key evil-normal-state-map (kbd "SPC c c") 'compile)
(define-key c++-mode-map (kbd "C-c C-k") 'mode-compile-kill)
(define-key evil-normal-state-map (kbd "SPC c k") 'mode-compile-kill)
(define-key c++-mode-map (kbd "C-c C-r") 'recompile)
(define-key evil-normal-state-map (kbd "SPC c r") 'recompile)

(add-hook 'c++-mode-hook 'private-c-c++-mode-hook)

(defun private-cperl-mode-hook ()
	;; (defun cperl-mode-insert-lcurly ()
	;;   (interactive)
	;;   (insert "{")
	;;   (let ((pps (syntax-ppss)))
	;; 	(when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
	;; 	  (c-indent-line)
	;; 	  (insert "\n\n}")
	;; 	  (c-indent-line)
	;; 	  (forward-line -1)
	;; 	  (c-indent-line))))
	;; (define-key global-map "{" 'cperl-mode-insert-lcurly)
	(define-key global-map (kbd "C-c C-c") 'compile)
	(define-key evil-normal-state-map (kbd "SPC c c") 'compile)
	(define-key global-map (kbd "C-c C-k") 'mode-compile-kill)
	(define-key evil-normal-state-map (kbd "SPC c k") 'mode-compile-kill)
	(define-key global-map (kbd "C-c C-r") 'recompile)
	(define-key evil-normal-state-map (kbd "SPC c r") 'recompile)
)
(add-hook 'cperl-mode-hook 'private-cperl-mode-hook)

(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(defun load-company-ess ()
)
(add-hook 'R-mode-hook 'load-company-ess)

(require 'projectile)
(projectile-global-mode 1)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

(require 'helm-projectile)
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p p") 'helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "C-c p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file-dwim)
(define-key projectile-mode-map (kbd "C-c p r") 'helm-projectile-recentf)
(define-key evil-normal-state-map (kbd "SPC p r") 'helm-projectile-recentf)
(define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
(define-key evil-normal-state-map (kbd "SPC p g") 'helm-projectile-grep)
(define-key projectile-mode-map (kbd "C-c p a") 'helm-projectile-ag)
(define-key evil-normal-state-map (kbd "SPC p a") 'helm-projectile-ag)
(define-key projectile-mode-map (kbd "C-c p o") 'helm-projectile-find-other-file)
(define-key evil-normal-state-map (kbd "SPC p o") 'helm-projectile-find-other-file)
(define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)

(define-key evil-normal-state-map (kbd "SPC \\") 'helm-projectile-find-file-dwim)
(define-key evil-normal-state-map (kbd "SPC \`") 'helm-projectile-switch-project)

(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

(setq gdb-many-windows t)
(setq gdb-show-main t)

; M-x semantic-force-refresh
;(ede-cpp-root-project "project_root"
                      ;:file "/dir/to/project_root/Makefile"
                      ;:include-path '("/include1"
                                      ;"/include2") ;; add more include
					  ; paths here
                      ;:system-include-path '("~/linux"))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(setq undo-tree-history-dir (let ((dir (concat user-emacs-directory
					       "undo-tree-history/")))
			      (make-directory dir :parents)
			      dir))
(setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-dir)))

(advice-add 'undo-tree-make-history-save-file-name :filter-return
	    (lambda (return-val) (concat return-val ".gz")))

(setq undo-tree-auto-save-history t)

;(defun my-emacs-lisp-mode-hook ()
  ;(highlight-indentation)
  ;(set-face-background 'highlight-indentation-face "#303030"))
;(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; (add-hook 'window-scroll-functions 'update-linum-format nil t)
;; (defun update-linum-format (window start)
;;     (interactive)
;;     (setq linum-format "%9d "))

(defvar endless/margin-display
    `((margin left-margin) ,(propertize "-----" 'face 'linum))
	  "String used on the margin.")

(defvar-local endless/margin-overlays nil
  "List of overlays in current buffer.")

(defun endless/setup-margin-overlays ()
  "Put overlays on each line which is visually wrapped."
  (interactive)
  (let ((ww (- (window-width)
               (if (= 0 (or (cdr fringe-mode) 1)) 1 0)))
        ov)
    (mapc #'delete-overlay endless/margin-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (null (eobp))
        ;; On each logical line
        (forward-line 1)
        (save-excursion
          (forward-char -1)
          ;; Check if it has multiple visual lines.
          (while (>= (current-column) ww)
            (endles/make-overlay-at (point))
            (forward-char (- ww))))))))

(defun endles/make-overlay-at (p)
  "Create a margin overlay at position P."
  (push (make-overlay p (1+ p)) endless/margin-overlays)
  (overlay-put
   (car endless/margin-overlays) 'before-string
   (propertize " "  'display endless/margin-display)))

; (add-hook 'linum-before-numbering-hook #'endless/setup-margin-overlays)

;; Set default splitting approach to vertical split
;(setq split-height-threshold nil)
;(setq split-width-threshold 80)

;(require 'golden-ratio)
;(golden-ratio-mode 1)

;; Line wrapping settings
(setq-default truncate-lines nil)
(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode 1)

(require 'history)
(history-mode 1)
(define-key global-map (kbd "C-x .") 'history-next-history)
(define-key global-map (kbd "C-x ,") 'history-prev-history)
(define-key global-map (kbd "C-x /") 'history-add-history)

(paredit-mode 1)
(define-key global-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key global-map (kbd "M-(") 'paredit-backward-slurp-sexp)
(define-key global-map (kbd "M-}") 'paredit-forward-barf-sexp)
(define-key global-map (kbd "M-{") 'paredit-backward-barf-sexp)

;(show-paren-mode 1)

(require 'highlight-parentheses)
(global-highlight-parentheses-mode 1)

(defun beginning-of-indentation-or-line ()
  "Move point to the beginning of text on the current line; if that is already
   the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-indentation-or-line)

(add-hook 'w3m-mode-hook (lambda ()
			   (indent-guide-mode 0)
			   (evil-normal-state)
			   (evil-define-key 'normal w3m-mode-map (kbd "RET") 'w3m-view-this-url)
			   (evil-define-key 'normal w3m-mode-map (kbd "q") 'w3m-close-window)
			   ))
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;quick access hacker news
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;;quick access reddit
(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
                (read-string "Enter the reddit (default: Linux): " nil nil "Linux" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
  )

;;i need this often
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site))) 
 
(setq helm-dash-docsets-path (expand-file-name "~/.emacs.d/docsets"))
(setq helm-dash-common-docsets '("C" "C++" "Perl" "Python 2" "Python 3" "Clojure" "R"))

(global-set-key (kbd "C-c h d") 'helm-dash)
(define-key evil-normal-state-map (kbd "SPC h d") 'helm-dash)

;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(require 'langtool)
(setq langtool-language-tool-jar "~/LanguageTool-3.0/languagetool-commandline.jar")
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
 
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "xpdf -g %n %o %b")))

(define-key evil-normal-state-map (kbd "C-_") '(lambda () (interactive)
						 (if (eq linum-format 'linum-format-func)
						     (setq linum-format 'linum-relative)
						   (setq linum-format 'linum-format-func))))

(require 'evil-magit)
(evil-define-key evil-magit-state magit-mode-map "=" 'magit-diff-less-context)
(define-key magit-log-mode-map (kbd "TAB") 'magit-cycle-margin-style)

(require 'gud)

(add-hook 'gdb-mode-hook '(lambda () (nolinum)))

(global-set-key (kbd "C-c d g") 'gdb)
(global-set-key (kbd "C-c d r") 'gud-run)
(global-set-key (kbd "C-c d n") 'gud-next)
(global-set-key (kbd "C-c d s") 'gud-step)
(global-set-key (kbd "C-c d b") 'gud-break)
(global-set-key (kbd "C-c d c") 'gud-cont)

(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
	(overlay-put ov 'face 'secondary-selection)
	ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
	 (bf (gud-find-file true-file)))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov (line-beginning-position) (line-end-position)
		    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (eq major-mode 'gud-mode)
	(delete-overlay gud-overlay)))

(require 'calc-bin)

(defun convert-region-base-m-to-n (m n)
  (lambda (start end)
    (interactive "r")
    (let
	((out
	  (convert-base-m-to-n m n (buffer-substring start end))))
      (delete-region start end)
      (insert out)
      )
    )
  )

(defun convert-region-list-base-m-to-n (m n)
  (lambda (start end)
    (interactive "r")
    (let
	((out
	  (s-join " " (mapcar (lambda (str) (convert-base-m-to-n m n str))
			      (split-string (buffer-substring start end) " ")))))
      (delete-region start end)
      (insert out)
      )
    )
  )

(defun convert-base-m-to-n (m n str)
  (let ((calc-number-radix n)
	(m-digit-num (length str)))
    (format "%s%s"
	    (make-string
	     (- (length (math-format-radix (- (expt m m-digit-num) 1)))
		(length (math-format-radix (string-to-number str m))))
	     ?0)
	    (math-format-radix (string-to-number str m))))
  )

(defun base-keyword (base)
  (if (eq base 10)
      ?d
    (if (eq base 16)
	?h
      (+ base #x30)
      )
    )
  )

(loop for m in '(2 8 10 16)
      do (loop for n in '(2 8 10 16)
	    do
	    ( let ((m-keyword (base-keyword m)) (n-keyword (base-keyword n)))
	     (global-set-key (kbd (format "C-c b %c %c" m-keyword n-keyword)) (convert-region-base-m-to-n m n))
	     (define-key evil-visual-state-map
	       (kbd (format "SPC i c %c %c" m-keyword n-keyword))
	       (convert-region-base-m-to-n m n))
	     (global-set-key (kbd (format "C-c b l %c %c" m-keyword n-keyword)) (convert-region-list-base-m-to-n m n))
	     (define-key evil-visual-state-map
	       (kbd (format "SPC i c l %c %c" m-keyword n-keyword))
	       (convert-region-list-base-m-to-n m n))
	     )))

(global-set-key (kbd "C-c b h 2") (convert-region-base-m-to-n 16 2))

(defun calc-eval-region (start end)
  (interactive "r")
  (let
      ((out
	(calc-eval (buffer-substring start end))))
    (delete-region start end)
    (insert out)
    )
  )

(global-set-key (kbd "C-c c C-x C-e") 'calc-eval-region)
(define-key evil-visual-state-map (kbd "SPC i c x e") 'calc-eval-region)

(defun buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.
   A binary buffer is defined as containing at least on null byte.
   Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current bufferis binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (let ((file-ext (file-name-extension (buffer-name))))
    (when
      (and
       (buffer-binary-p)
       (not (string= file-ext "png"))
       (not (string= file-ext "jpg"))
       (not (string= file-ext "jpeg"))
	   )
      (hexl-mode)))))

(add-hook 'find-file-hooks 'hexl-if-binary)

(defalias 'yes-or-no-p 'y-or-n-p)

;; XXX: an upstream bug: the first and-clause determining whether two files are equal
;; in func. ztree-diff-model-files-equal of ztree-diff-model.el should
;; be removed
(require 'ztree)
(define-key evil-normal-state-map (kbd "SPC f d d") 'ztree-diff)

(eval-after-load "ztree"
  '(progn
     (evil-define-key 'motion ztree-mode-map (kbd "TAB") 'ztree-perform-soft-action)
     (evil-define-key 'motion ztree-mode-map (kbd "RET") 'ztree-perform-action)
     (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-jump-side)
     (evil-define-key 'motion ztree-mode-map (kbd "l") 'ztree-jump-side)
     (evil-define-key 'motion ztree-mode-map (kbd "r") 'ztree-diff-partial-rescan)
     (evil-define-key 'motion ztree-mode-map (kbd "R") 'ztree-diff-full-rescan)
     (evil-define-key 'motion ztree-mode-map (kbd "h") 'ztree-diff-toggle-show-equal-files)
     (evil-define-key 'motion ztree-mode-map (kbd "v") 'ztree-diff-view-file)
     (evil-define-key 'motion ztree-mode-map (kbd "o") 'ztree-diff-view-file)
     (evil-define-key 'motion ztree-mode-map (kbd "q") 'quit-window)
     ))

(eval-after-load "projectile"
  '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
					  (substring dir 0 -1))
					(remove-if-not (lambda (project)
							 (file-directory-p (concat project "/.git/")))
						       (projectile-relevant-known-projects))))

	  (setq magit-repo-dirs-depth 1)))

(require 'fill-column-indicator)
(define-globalized-minor-mode fci-global-mode fci-mode turn-on-fci-mode)
(setq fci-rule-column 74)
(fci-global-mode -1)

(require 'indent-guide)
(setq indent-guide-recursive t)
(setq indent-guide-char "|")
(indent-guide-global-mode -1)

(define-key evil-normal-state-map (kbd "SPC i g 1") (lambda () (interactive)
						    (fci-global-mode)
						    (indent-guide-global-mode)
						    ))
(define-key evil-normal-state-map (kbd "SPC i g 0") (lambda () (interactive)
						    (fci-global-mode -1)
						    (indent-guide-global-mode -1)
						    ))


; TODO: no undo trace
(define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive)
			      (evil-delete-line (- (line-beginning-position) 1) (line-end-position) t)
			      (evil-previous-line)
			      (evil-end-of-line)
			      (evil-paste-after 1)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-next-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-previous-line)
			      (beginning-of-line-text)
			      ))

(define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive)
			      (evil-delete-line (- (line-beginning-position) 1) (line-end-position) t)
			      (evil-next-line)
			      (evil-end-of-line)
			      (evil-paste-after 1)
			      (evil-previous-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (evil-next-line)
			      (evil-indent (line-beginning-position) (line-end-position))
			      (beginning-of-line-text)
			      ))

; TODO: review low-quality code
(define-key evil-visual-state-map (kbd "M-k") (lambda () (interactive)
						(let (
						      (region-line-num (count-lines (region-beginning) (region-end)))
						      )
						  (evil-delete-line (region-beginning) (- (region-end) 1) t)
						  (evil-previous-line 2)
						  (evil-paste-after 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  (evil-indent-line (region-beginning) (region-end))
						  (evil-next-line (- region-line-num 1))
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-next-line 1)
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-previous-line 1)
						  (evil-visual-line)
						  )
						))

(define-key evil-visual-state-map (kbd "M-j") (lambda () (interactive)
						(let (
						      (region-line-num (count-lines (region-beginning) (region-end)))
						      )
						  (evil-delete-line (region-beginning) (- (region-end) 1) t)
						  (evil-paste-after 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  (evil-indent-line (region-beginning) (region-end))
						  (evil-next-line (- region-line-num 1))
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-previous-line region-line-num)
						  (evil-indent (line-beginning-position) (line-end-position))
						  (evil-next-line 1)
						  (evil-visual-line)
						  (evil-next-line (- region-line-num 1))
						  )
						))

(global-git-gutter-mode 1)

(setq git-gutter:ask-p nil)

(setq git-gutter:added-sign " + ")
(setq git-gutter:deleted-sign " - ")
(setq git-gutter:modified-sign " * ")

(global-set-key (kbd "C-c m g TAB") 'global-git-gutter-mode)
(define-key evil-normal-state-map (kbd "SPC m g TAB") 'global-git-gutter-mode)

(global-set-key (kbd "C-c m g g") 'git-gutter)
(define-key evil-normal-state-map (kbd "SPC m g g") 'git-gutter)
(global-set-key (kbd "C-c m g SPC") 'git-gutter:popup-hunk)
(define-key evil-normal-state-map (kbd "SPC m g SPC") 'git-gutter:popup-hunk)

(global-set-key (kbd "C-c m g s") 'git-gutter:stage-hunk)
(define-key evil-normal-state-map (kbd "SPC m g s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-c m g d") 'git-gutter:revert-hunk)
(define-key evil-normal-state-map (kbd "SPC m g d") 'git-gutter:popup-diff)
(global-set-key (kbd "C-c m g r") 'git-gutter:popup-diff)
(define-key evil-normal-state-map (kbd "SPC m g r") 'git-gutter:revert-hunk)

(global-set-key (kbd "C-c m g n") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "SPC m g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c m g j") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "SPC m g j") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c m g p") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "SPC m g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c m g k") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "SPC m g k") 'git-gutter:previous-hunk)

(global-set-key (kbd "C-x t t") 'transpose-frame)
(define-key evil-normal-state-map (kbd "SPC x t t") 'transpose-frame)
(global-set-key (kbd "C-x t j") 'flip-frame)
(define-key evil-normal-state-map (kbd "SPC x t j") 'flip-frame)
(global-set-key (kbd "C-x t k") 'flip-frame)
(define-key evil-normal-state-map (kbd "SPC x t k") 'flip-frame)
(global-set-key (kbd "C-x t h") 'flop-frame)
(define-key evil-normal-state-map (kbd "SPC x t h") 'flop-frame)
(global-set-key (kbd "C-x t l") 'flop-frame)
(define-key evil-normal-state-map (kbd "SPC x t l") 'flop-frame)
(global-set-key (kbd "C-x t r r") 'rotate-frame)
(define-key evil-normal-state-map (kbd "SPC x t r r") 'rotate-frame)
(global-set-key (kbd "C-x t r l") 'rotate-frame-clockwise)
(define-key evil-normal-state-map (kbd "SPC x t r l") 'rotate-frame-clockwise)
(global-set-key (kbd "C-x t r h") 'rotate-frame-anticlockwise)
(define-key evil-normal-state-map (kbd "SPC x t r h") 'rotate-frame-anticlockwise)

(define-key evil-normal-state-map (kbd "SPC i a a") 'artist-mode)

(evil-define-key 'normal artist-mode-map (kbd "RET") 'artist-key-set-point)
(evil-define-key 'normal artist-mode-map (kbd "j") 'artist-next-line)
(evil-define-key 'normal artist-mode-map (kbd "k") 'artist-previous-line)
(evil-define-key 'normal artist-mode-map (kbd "h") 'artist-backward-char)
(evil-define-key 'normal artist-mode-map (kbd "l") 'artist-forward-char)
(evil-define-key 'normal artist-mode-map (kbd "<") 'artist-toggle-first-arrow)
(evil-define-key 'normal artist-mode-map (kbd ">") 'artist-toggle-second-arrow)

(evil-define-key 'normal artist-mode-map (kbd "SPC i a o") 'artist-select-operation)
(evil-define-key 'normal artist-mode-map (kbd "SPC i a r") 'artist-select-op-rectangle)
(evil-define-key 'normal artist-mode-map (kbd "SPC i a l") 'artist-select-op-poly-line)

(require 'company-statistics)
(company-statistics-mode)

(require 'ecb)

(setq ecb-tip-of-the-day nil)

(ecb-layout-define "right-side-simplistic" right nil
		   (ecb-split-ver 0.696969696969697 t)
		   (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
		   (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
		   (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
		   (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
		   (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
		   )

(setq ecb-windows-width 35)
(ecb-layout-switch "right-side-simplistic")

(setq ecb-highlight-token-with-point t)
(setq ecb-auto-expand-token-tree t)
(setq expand-methods-switch-off-auto-expand t)

;; ECB does not provide a major mode to bind key-bindings,
;; so use motion-state instead
(define-key evil-motion-state-map (kbd "RET") nil)
;; move evil-ret onto evil normal state
(define-key evil-normal-state-map (kbd "RET") 'evil-ret)
(add-hook 'ecb-history-buffer-after-create-hook 'evil-motion-state)
(add-hook 'ecb-directories-buffer-after-create-hook 'evil-motion-state)
(add-hook 'ecb-methods-buffer-after-create-hook 'evil-motion-state)
(add-hook 'ecb-sources-buffer-after-create-hook 'evil-motion-state)

(global-set-key (kbd "C-M-\\") 'ecb-toggle-ecb-windows)

(global-set-key (kbd "C-c e m") 'ecb-goto-window-methods)
(define-key evil-normal-state-map (kbd "SPC e m") 'ecb-goto-window-methods)
(global-set-key (kbd "C-c e c") 'ecb-clear-history)
(define-key evil-normal-state-map (kbd "SPC e c") 'ecb-clear-history)
(global-set-key (kbd "C-c e RET") 'ecb-expand-methods-nodes)
(define-key evil-normal-state-map (kbd "SPC e RET") 'ecb-expand-methods-nodes)

(ecb-activate)
(ecb-hide-ecb-windows)

(define-key evil-normal-state-map (kbd "SPC i 3") (lambda () (interactive)
						    (evil-scroll-line-to-center (line-number-at-pos))
						    (split-window-right)
						    (other-window 1)
						    (evil-scroll-line-to-center (line-number-at-pos))
						    (evil-window-bottom 1)
						    (evil-scroll-line-to-top (line-number-at-pos))
						    (evil-window-middle)
						    (other-window 1)
						    ))
(define-key evil-normal-state-map (kbd "C-M-e") (lambda () (interactive)
						  (other-window 1)
						  (evil-scroll-line-down 1)
						  (other-window 1)
						  (evil-scroll-line-down 1)
						  ))
(define-key evil-normal-state-map (kbd "C-M-y") (lambda () (interactive)
						  (other-window 1)
						  (evil-scroll-line-up 1)
						  (other-window 1)
						  (evil-scroll-line-up 1)
						  ))
