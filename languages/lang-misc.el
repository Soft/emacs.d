;;; lang-misc.el --- Miscellaneous languages -*- lexical-binding: t -*-

;;; Commentary:

;; Miscellaneous languages.

;;; Code:

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package nim-mode
  :mode (("\\.nim\\'" . nim-mode))
  :ensure t)

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :ensure t)

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :ensure t)

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter "ruby")

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :interpreter ("php" . php-mode)
  :ensure t)

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :interpreter ("scala" . scala-mode)
  :ensure t)

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode)
         ("/Jenkinsfile" . groovy-mode))
  :ensure t)

(use-package qml-mode
  :mode (("\\.qml\\'" . qml-mode))
  :interpreter ("qml" . qml-mode)
  :ensure t)

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :ensure t)

(defun adq/twee-setup ()
  "Defaults for Twee."
  (gll-guess-language-lite-mode))

(use-package twee-mode
  :commands (twee-mode)
  :mode (("\\.tw\\'" . twee-mode)
         ("\\.twee\\'" . twee-mode))
  :init
  (add-hook 'twee-mode-hook #'adq/twee-setup))

(provide 'lang-misc)

;;; lang-misc.el ends here
