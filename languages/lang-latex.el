;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; LaTeX and related technologies

(defun latex-setup ()
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (TeX-fold-mode)
  (TeX-fold-buffer))

(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :defer t
  :ensure t)

(use-package tex-mode
  :mode (("\\.tex\\'" . LaTeX-mode))
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'latex-setup))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'" . bibtex-mode)))

(provide 'lang-latex)
