;;; lang-latex.el --- LaTeX -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for wiriting LaTeX documents.

;;; Code:

(defun latex-setup ()
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (TeX-fold-mode)
  (TeX-fold-buffer))

(use-package tex-mode
  :mode (("\\.tex\\'" . LaTeX-mode))
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'latex-setup))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'" . bibtex-mode)))

(provide 'lang-latex)

;;; lang-latex.el ends here
