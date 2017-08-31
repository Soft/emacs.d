;; -*- mode: Emacs-Lisp; lexical-binding: t; coding: utf-8 -*-
;; Python

(use-package pyvenv
  :ensure t
  :defer t)

(use-package yapfify
  :if (programs-p "yapf")
  :ensure t
  :defer t)

(defun python-setup ()
  "Defaults for Python."
  (highlight-indent-guides-mode)
  (pyvenv-mode))

(define-skeleton python-doc-comment
  "Insert Python doc comment" nil
  > "\"\"\"" _ "\"\"\"")

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :init (add-hook 'python-mode-hook #'python-setup)
  :config
  (bind-keys
   :map python-mode-map
   ("M-\"" . python-doc-comment)))

(provide 'lang-python)
