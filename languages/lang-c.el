;;; lang-c-el --- C/C++ -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for C and C-styla languages.

;;; Code:

(provide 'lang-c)

(defvar adq/c-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("->" . ?→)
    ("..." . ?…))
  "Symbol prettification alist for `c-mode'.")

(defun adq/c-setup ()
  (setq-local prettify-symbols-alist adq/c-prettify-symbols-alist))

(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-hook #'adq/c-setup))

;;; lang-c.el ends here
