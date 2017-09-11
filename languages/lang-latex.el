;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; LaTeX and related technologies

(defun latex-setup ()
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (TeX-fold-mode)
  (TeX-fold-buffer))

(defun pdf-view-setup ()
  (pdf-tools-enable-minor-modes)
  (pdf-view-fit-page-to-window)
  (auto-revert-mode 1))

(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-view-setup)
  (bind-keys
   :map pdf-view-mode-map
   ("j" . pdf-view-next-line-or-next-page)
   ("k" . pdf-view-previous-line-or-previous-page)
   ("J" . pdf-view-next-page)
   ("K" . pdf-view-previous-page)))

(use-package tex-mode
  :mode (("\\.tex\\'" . LaTeX-mode))
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'latex-setup))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'" . bibtex-mode)))

(provide 'lang-latex)
