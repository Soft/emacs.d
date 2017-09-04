;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Miscellaneous languages

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode))
  :ensure t)

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :ensure t)

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :ensure t)

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :ensure t)

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter "ruby"
  :ensure t)

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :interpreter ("php" . php-mode)
  :ensure t)

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :interpreter ("scala" . scala-mode)
  :ensure t)

(use-package ebuild-mode
  :mode (("\\.ebuild\\'" . ebuild-mode)))

(provide 'lang-misc)
