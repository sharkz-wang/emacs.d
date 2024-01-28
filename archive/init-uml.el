(require-package 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(require-package 'org)
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(require 'ob-plantuml)
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq plantuml-jar-path "~/plantuml.jar")

(provide 'init-uml)
