
(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :config
  (use-package haskell-font-lock)
  (setq-default haskell-interactive-popup-errors nil
                haskell-font-ock-symbols t))

(provide 'lang-haskell)
