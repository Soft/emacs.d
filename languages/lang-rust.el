;;; lang-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Rust programming environment

;;; Code:

(defun adq/rust-setup ()
  "Defaults for Rust."
  (racer-mode)
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
