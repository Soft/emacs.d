;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Rust configuration

(defun rust-setup ()
  "Defaults for Rust."
  (racer-mode)
  (cargo-minor-mode))

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
  (add-hook 'rust-mode-hook #'rust-setup))

(provide 'lang-rust)

