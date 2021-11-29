;;; env-haskell.el -*- lexical-binding: t; -*-

(use-package lsp-haskell :defer t)

(defvar adq/haskell-extra-font-lock-symbols-alist
  '((">>" . "≫")
    ("<<" . "≪")
    ;; (">>=" . "↪")
    ;; ("=<<" . "↩")
    ;; (">>>" . "⋙")
    ;; ("<<<" . "⋘")
    ;; ("<~" . "⇜")
    ;; ("++" . "⧺")
    ;; ("+++" . "⧻")
    ;; ("***" . "⁂")
    ;; ("<>" . "⊕")
    ;; ("<*>" . "⊛")
    ;; ("<|>" . "⟐")
    ;; ("<$>" . "↥")
    ;; ("*>" . "⩺")
    ;; ("<*" . "⩹")
    )
  "Symbol prettification alist for `haskell-mode'. This will be
  appended to the default list provided by `haskell-mode'.")

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.ghci\\'" . ghci-script-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (setq haskell-font-lock-symbols t)
  (setq haskell-font-lock-symbols-alist
        (append haskell-font-lock-symbols-alist adq/haskell-extra-font-lock-symbols-alist)))

(provide 'env-haskell)
