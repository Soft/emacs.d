;; -*- mode: Emacs-Lisp; lexical-binding: t; coding: utf-8 -*-
;; Python

(use-package pyvenv
  :ensure t
  :defer t)

(use-package yapfify
  :ensure t
  :defer t)

(defun python-setup ()
  "Defaults for Python."
  (highlight-indent-guides-mode)
  (pyvenv-mode))

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :init (add-hook 'python-mode-hook #'python-setup))

(provide 'lang-python)
