;;; env-cc.el -*- lexical-binding: t; -*-

(use-package clang-format
  :after cc-mode
  :bind
  (:map
   c-mode-map
   ("C-c c f" . clang-format-buffer)
   :map
   c++-mode-map
   ("C-c c f" . clang-format-buffer)))

(defvar adq/c-prettify-symbols-alist
  '(("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("->" . ?→)
    ("..." . ?…)
    ("*" . ?∗)
    ("||" . ?‖))
  "Symbol prettification alist for `c-mode'.")

(defvar adq/c++-prettify-symbols-alist
  '(("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("->" . ?→)
    ("..." . ?…)
    ("*" . ?∗)
    ("::" . ?∷)
    ("||" . ?‖))
  "Symbol prettification alist for `c++-mode'.")

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)
  :diminish modern-c++-font-lock-mode)

(defun adq/c-setup ()
  "Defaults for C."
  (setq-local prettify-symbols-alist
              adq/c-prettify-symbols-alist))


(defun adq/c++-setup ()
  "Defaults for C++."
  (setq-local prettify-symbols-alist
              adq/c++-prettify-symbols-alist))

(use-package cc-mode
  :straight nil
  :defer t
  :config
  (add-hook 'c-mode-hook #'adq/c-setup)
  (add-hook 'c++-mode-hook #'adq/c++-setup))

(provide 'env-cc)
