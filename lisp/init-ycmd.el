;; note: why choosed ycmd: subsequnce completion
;; minor drawback: a lot of garbage entries (e.g., macros)

(require-package 'ycmd)
(require-package 'company-ycmd)

;; ycmd Installation
;; 1) git clone https://github.com/Valloric/ycmd.git
;; 2) git submodule update --init --recursive
;; 3) install mono-xbuild and mono-devel
;; 4) ./build.py --clang-completer --omnisharp-completer --gocode-completer --system-libclang
;; 5) appending search path into `.ycm_extra_conf.py'
;;    for example,
;;    [ "-system",
;;      "/usr/include/stdio.h" ]

(add-hook 'company-mode-hook (lambda ()
	    (company-ycmd-setup)
	    (global-ycmd-mode)))

(setq ycmd-force-semantic-completion t)
(set-variable 'ycmd-server-command (list "python" (expand-file-name "/opt/ycmd/ycmd")))
(set-variable 'ycmd-global-config "/opt/ycmd/cpp/ycm/.ycm_extra_conf.py")

(setq request-message-level -1)

(provide 'init-ycmd)
