;;; lang-haskell.el --- Haskell -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for Haskell programming.

;;; Code:

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :config
  (use-package haskell-font-lock)
  (setq-default haskell-interactive-popup-errors nil
                haskell-font-lock-symbols t))

(provide 'lang-haskell)

;;; lang-haskell.el ends here
