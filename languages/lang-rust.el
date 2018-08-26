;;; lang-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Rust programming environment

;;; Code:

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
  (when (featurep 'flycheck)
    (flycheck-rust-setup))
  (when (getenv "RUST_SRC_PATH")
    (racer-mode))
  (setq-local prettify-symbols-alist adq/rust-prettify-symbols-alist)
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

(use-package flycheck-rust
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)
         ("\\.lalrpop\\'" . rust-mode))
  :init
  (add-hook 'rust-mode-hook #'adq/rust-setup)
  :bind
  (:map rust-mode-map
        ("C-c a =" . rust-format-buffer)
        ("C-c a a" . cargo-process-run)
        ("C-c a b" . cargo-process-build)
        ("C-c a e" . cargo-process-bench)
        ("C-c a l" . cargo-process-clean)
        ("C-c a d" . cargo-process-doc)
        ("C-c a D" . cargo-process-doc-open)
        ("C-c a t" . cargo-process-test)
        ("C-c a T" . cargo-process-current-test)
        ("C-c a u" . cargo-process-update)
        ("C-c a s" . cargo-process-search)
        ("C-c a c" . cargo-process-check)
        ("C-c a C" . cargo-process-clippy)))

(provide 'lang-rust)

;;; lang-rust.el ends here
