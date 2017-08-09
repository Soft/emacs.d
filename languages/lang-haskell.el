
(use-package haskell-mode
  :ensure t
  :config
  (use-package haskell-font-lock)
  (setq-default haskell-interactive-popup-errors nil
                haskell-font-ock-symbols t))

(provide 'lang-haskell)
