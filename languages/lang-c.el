;;; lang-c-el --- C/C++ -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for C and C-styla languages.

;;; Code:

(defvar adq/c-prettify-symbols-alist
  '(("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("->" . ?→)
    ("..." . ?…)
    ("||" . ?‖))
  "Symbol prettification alist for `c-mode'.")

(defvar adq/c++-prettify-symbols-alist
  '(("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("->" . ?→)
    ("::" . ?∷)
    ("..." . ?…)
    ("||" . ?‖))
  "Symbol prettification alist for `c++-mode'.")

(use-package clang-format
  :if (adq/programs-p "clang-format")
  :ensure t
  :defer t)

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :diminish modern-c++-font-lock-mode)

(defun adq/c-setup ()
  "Defaults for C."
  (setq-local prettify-symbols-alist adq/c-prettify-symbols-alist))


(defun adq/c++-setup ()
  "Defaults for C++."
  (modern-c++-font-lock-mode)
  (setq-local prettify-symbols-alist adq/c++-prettify-symbols-alist))

(adq/region-switch-command adq/clang-format-region-or-buffer
  #'clang-format-region #'clang-format-buffer
  "Use clang-format to format region or buffer.")

(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-hook #'adq/c-setup)
  (add-hook 'c++-mode-hook #'adq/c++-setup)
  :bind
  (
   :map c++-mode-map
   ("C-c a o" . ff-find-other-file)
   ("C-c a =" . adq/clang-format-region-or-buffer)
   :map c-mode-map
   ("C-c a o" . ff-find-other-file)
   ("C-c a =" . adq/clang-format-region-or-buffer)))

(provide 'lang-c)

;;; lang-c.el ends here
