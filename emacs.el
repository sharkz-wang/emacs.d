;; -*- lexical-binding: t; -*-

;; set recipes
 (setq
 el-get-sources
 '(
   ;; Vim-emulation
   (:name evil-org-mode
	  :type git
	  :url "https://github.com/edwtjo/evil-org-mode")

   (:name transpose-frame)

   (:name undo-tree)
   (:name paredit)
   (:name highlight-parentheses)

   (:name smooth-scroll)

   (:name company-statistics)
   (:name emacs-ycmd
	  :type git
	  :url "https://github.com/abingham/emacs-ycmd")
   (:name cedet)
   (:name ecb)
   (:name yasnippet)

   (:name elpy)

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
   (:name helm-gtags)

   (:name uncrustify-mode
		:type git
		:url "https://github.com/koko1000ban/emacs-uncrustify-mode")

   (:name dtrt-indent)

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


(global-set-key (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "C-x C-o") 'ff-find-other-file)

(setq-default indent-tabs-mode t)
(setq c-default-style "k&r")
(setq-default tab-width 8)
;(defvaralias 'c-basic-offset 'tab-width)

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

(global-set-key (kbd "C-x C-d") 'ediff-buffers)
(define-key evil-normal-state-map (kbd "SPC x d") 'ediff-buffers)

(global-set-key (kbd "C-x D") 'ediff-files)
(define-key evil-normal-state-map (kbd "SPC f d f") 'ediff-files)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

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

(add-hook 'org-mode-hook
	  (lambda () (modify-syntax-entry ?_ "w")
	    (add-to-list 'org-modules "org-habit")))
(add-hook 'org-capture-mode-hook
	  (lambda () (evil-emacs-state)))
(add-hook 'c-mode-hook
    (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook
    (lambda () (modify-syntax-entry ?_ "w")))
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

(require 'org)
(setq org-startup-indented 1)
(setq org-clock-sound t)
(setq org-timer-default-timer 25)

(custom-set-faces
  '(org-todo ((t :foreground "#FF1493" :weight bold))))

(custom-set-faces
 `(company-tooltip-selection ((t (:foreground ,"#F5F5F5" :background ,"#444444"))))
 `(company-tooltip-common-selection ((t (:foreground ,"#F5F5F5" :background ,"#444444"))))
 `(company-tooltip-common ((t (:foreground ,"#F5F5F5" :background ,"#444444")))))

(hl-spotlight-mode 1)
(setq hl-spotlight-height 0)
(custom-set-faces
  '(hl-spotlight ((t :inherit highlight :weight bold))))

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


(global-set-key (kbd "C-c h o") 'helm-org-agenda-files-headings)

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

;; Line wrapping settings
(setq-default truncate-lines nil)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode 1)

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

(require 'gud)

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

;; XXX: an upstream bug: the first and-clause determining whether two files are equal
;; in func. ztree-diff-model-files-equal of ztree-diff-model.el should
;; be removed

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
