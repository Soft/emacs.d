;;; env-tex.el -*- lexical-binding: t -*-

(defun adq/latex-setup ()
  "Setup buffer for LaTeX."
  (add-to-list 'LaTeX-verbatim-environments "minted"))

(use-package auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (add-hook 'LaTeX-mode-hook #'adq/latex-setup))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'" . bibtex-mode)))

(provide 'env-tex)
