(require-package 'test-c)

(eval-after-load 'test-c
  '(progn
     ;; update the default command to use clang
     (setq test-c-default-compile-command "clang -Wall $src -o $exe")
     ;; print error code after running
     (setq test-c-default-run-command "$exe ; echo $?")
     ;; use simpler skeleton
     (setq test-c-default-code "
#include <stdio.h>

int main (void) {

        return 0;
}
")
     ))

(defun init-cc-handler ()

  (require 'init-company)
  (require 'init-gtags)
  (require 'init-semantic)

  ;; XXX: (require-package 'hierarchy) does not work, why?
  (load "hierarchy.el")
  (require-package 'call-graph)

  (c-set-style "linux")
  (custom-set-variables '(c-basic-offset '8))

  (helm-gtags-mode 1)

  (setq insert-print 'c-insert-print)
  (setq insert-for-loop 'c-insert-for-loop)
 
  (define-key evil-normal-state-map (kbd "SPC d c") 'call-graph)
  
  (define-key c-mode-base-map "{" 'c-mode-insert-lcurly)
  (define-key c-mode-base-map (kbd "SPC m s") 'c-style-selector)
  )

(add-hook 'c-mode-hook 'init-cc-handler)
(add-hook 'c++-mode-hook 'init-cc-handler)

(provide 'init-c)
