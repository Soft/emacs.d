
(use-package toml-mode
  :ensure t)

(use-package racer
  :ensure t)

(use-package cargo
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (progn
    (racer-mode)
    (cargo-minor-mode)))

(provide 'lang-rust)

