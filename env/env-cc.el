;;; env-cc.el -*- lexical-binding: t; -*-

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

(defun adq/c-setup ()
  "Defaults for C."
  (setq-local prettify-symbols-alist
              adq/c-prettify-symbols-alist))

(defun adq/c++-setup ()
  "Defaults for C++."
  (c-set-offset 'innamespace [0]) ; Do not indent namespaces
  (setq-local prettify-symbols-alist
              adq/c++-prettify-symbols-alist))

(use-package cc-mode
  :straight nil
  :mode (("\\.mm\\'" . objc-mode))
  :config
  (add-hook 'c-mode-hook #'adq/c-setup)
  (add-hook 'c++-mode-hook #'adq/c++-setup)
  (with-eval-after-load 'lsp-clangd
    (bind-keys
     :map c-mode-map
     ("C-c c o" . lsp-clangd-find-other-file)
     :map c++-mode-map
     ("C-c c o" . lsp-clangd-find-other-file))))

(provide 'env-cc)
