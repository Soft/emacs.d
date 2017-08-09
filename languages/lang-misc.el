;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Miscellaneous languages

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :defer t
  :ensure t)

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode))
  :defer t
  :ensure t)

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :defer t
  :ensure t)

(provide 'lang-misc)
