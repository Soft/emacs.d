;;; lang-c-el --- C/C++ -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for C and C-styla languages.

;;; Code:

(defvar adq/c-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("->" . ?→)
    ("=" . ?≔)
    ("*" . ?⁎)
    ("..." . ?…))
  "Symbol prettification alist for `c-mode'.")

(defvar adq/c++-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("->" . ?→)
    ("=" . ?≔)
    ("*" . ?⁎)
    ("..." . ?…))
  "Symbol prettification alist for `c++-mode'.")

(use-package clang-format
  :if (adq/programs-p "clang-format")
  :ensure t
  :defer t)

(defun adq/c-setup ()
  "Defaults for C."
  (setq-local prettify-symbols-alist adq/c-prettify-symbols-alist))


(defun adq/c++-setup ()
  "Defaults for C++."
  (setq-local prettify-symbols-alist adq/c++-prettify-symbols-alist))

(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-hook #'adq/c-setup)
  (add-hook 'c++-mode-hook #'adq/c++-setup))

(provide 'lang-c)

;;; lang-c.el ends here
