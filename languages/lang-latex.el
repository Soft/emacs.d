;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; LaTeX and related technologies

(defun latex-setup ()
  (add-to-list 'LaTeX-verbatim-environment "minted")
  (TeX-fold-mode)
  (TeX-fold-buffer))

(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :defer t
  :ensure t)

(use-package tex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :ensure auctex
  :init
  (add-hook 'tex-mode-hook #'latex-setup))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'" . bibtex-mode)))

(provide 'lang-latex)
