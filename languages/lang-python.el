;;; lang-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:

;; Python programming environment

;;; Code:

(use-package pyvenv
  :ensure t
  :defer t)

(use-package yapfify
  :if (adq/programs-p "yapf")
  :ensure t
  :defer t)

(defun adq/python-setup ()
  "Defaults for Python."
  (highlight-indent-guides-mode)
  (pyvenv-mode)
  (anaconda-eldoc-mode))

(define-skeleton python-doc-comment
  "Insert Python doc comment" nil
  > "\"\"\"" _ "\"\"\"" \n)

(use-package anaconda-mode
  :ensure t
  :defer t)

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :init (add-hook 'python-mode-hook #'adq/python-setup)
  :config
  (bind-keys
   :map python-mode-map
   ("M-\"" . python-doc-comment)))

(provide 'lang-python)

;;; lang-python.el ends here
