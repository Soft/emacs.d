;;; env-rust.el -*- lexical-binding: t; -*-

(defvar adq/rust-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("->" . ?→)
    ("=>" . ?⇒)
    (".." . ?‥)
    ("..." . ?…)
    ("::" . ?∷))
  "Symbol prettification alist for `rust-mode'.")

(defun adq/rust-setup ()
  "Defaults for Rust."
  (setq-local prettify-symbols-alist adq/rust-prettify-symbols-alist))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(use-package cargo :defer t)

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)
         ("\\.lalrpop\\'" . rust-mode))
  :config
  (add-hook 'rust-mode-hook #'adq/rust-setup))

(provide 'env-rust)
