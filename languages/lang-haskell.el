;;; lang-haskell.el --- Haskell -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for Haskell programming.

;;; Code:

(use-package hindent
  :if (adq/programs-p "hindent")
  :ensure t
  :defer t)

(use-package hlint-refactor
  :if (adq/programs-p "hlint")
  :ensure t
  :defer t)

(use-package hasky-stack
  :if (adq/programs-p "stack")
  :ensure t
  :defer t
  :config
  (setq hasky-stack-auto-target t))

(use-package intero
  :ensure t
  :defer t)

(defun adq/haskell-setup ()
  "Defaults for Haskell."
  (setq haskell-interactive-popup-errors nil
        haskell-font-lock-symbols t)
  (intero-mode))

(adq/region-switch-command adq/hindent-format-region-or-buffer
  #'hindent-reformat-region #'hindent-reformat-buffer
  "Use hindent to format region or buffer.")

;; FIXME: All the commands might not be present
(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :config
  (add-hook 'haskell-mode-hook  #'adq/haskell-setup)
  :bind
  (:map haskell-mode-map
        ("C-c a =" . adq/hindent-format-region-or-buffer)
        ("C-c a r" . hlint-refactor-refactor-at-point)
        ("C-c a R" . hlint-refactor-refactor-buffer)
        ("C-c a a" . hasky-stack-execute)
        ("C-c a c" . hasky-stack-package-action-popup)
        ("C-c a s" . haskell-sort-imports)
        ("C-c a e" . intero-restart)))

(provide 'lang-haskell)

;;; lang-haskell.el ends here
