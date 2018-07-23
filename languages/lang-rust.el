;;; lang-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Rust programming environment

;;; Code:

(defvar adq/rust-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("->" . ?→)
    (".." . ?‥)
    ("..." . ?…)
    ("::" . ?∷))
  "Symbol prettification alist for `rust-mode'.")

(defun adq/rust-setup ()
  "Defaults for Rust."
  (when (getenv "RUST_SRC_PATH")
    (racer-mode))
  (setq-local prettify-symbols-alist adq/rust-prettify-symbols-alist)
  (cargo-minor-mode)
  (eldoc-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode))
  :ensure t)

(use-package racer
  :defer t
  :ensure t)

(use-package cargo
  :defer t
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)
         ("\\.lalrpop\\'" . rust-mode))
  :init
  (add-hook 'rust-mode-hook #'adq/rust-setup))

(provide 'lang-rust)

;;; lang-rust.el ends here
