(require 'init-c-defs)
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

        fprintf(stderr, \"hello, world!\\n\");
        return 0;
}
")
     ))

;; XXX: temporarily move load statement out of `init-cc-handler',
;;      which causes redundant load every time opening a c file
;; FIXME: (require-package 'hierarchy) does not work, why?
(load "hierarchy.el")
(require-package 'call-graph)

(defun init-cc-handler ()

  (require 'init-company)
  (require 'init-semantic)

  (c-set-style "linux")
  (custom-set-variables '(c-basic-offset '8))

  (setq insert-print         'c-insert-print)
  (setq insert-debug         'c-insert-debug)
  (setq insert-for-loop      'c-insert-for-loop)
  (setq insert-todo-comment  'c-insert-todo-comment)
  (setq insert-fixme-comment 'c-insert-fixme-comment)
  (setq insert-xxx-comment   'c-insert-xxx-comment)
 
  (define-key evil-normal-state-map (kbd "SPC d c") 'call-graph)
  
  (define-key c-mode-base-map "{" 'c-mode-insert-lcurly)
  (define-key c-mode-base-map (kbd "SPC m s") 'c-style-selector)
  )

(add-hook 'c-mode-hook 'init-cc-handler)
(add-hook 'c++-mode-hook 'init-cc-handler)

(provide 'init-c)
