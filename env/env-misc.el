;;; env-misc.el -*- lexical-binding: t; -*-

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package nim-mode
  :mode (("\\.nim\\'" . nim-mode)))

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

(use-package ruby-mode
  :straight nil
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter "ruby")

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :interpreter ("php" . php-mode))

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :interpreter ("scala" . scala-mode))

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode)
         ("/Jenkinsfile" . groovy-mode)))

(use-package qml-mode
  :mode (("\\.qml\\'" . qml-mode))
  :interpreter ("qml" . qml-mode))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package docker :defer t)

(use-package dockerfile-mode
  :mode (("/Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :mode (("/docker-compose.*\.yml\\'" . docker-compose-mode)))

(provide 'env-misc)
