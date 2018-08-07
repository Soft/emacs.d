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

(defvar adq/haskell-extra-font-lock-symbols-alist
  '((">>" . "≫")
    ("<<" . "≪")
    (">>=" . "↪")
    ("=<<" . "↩")
    (">>>" . "⋙")
    ("<<<" . "⋘")
    ("<~" . "⇜")
    ("++" . "⧺")
    ("+++" . "⧻")
    ("***" . "⁂")
    ("<>" . "⊕")
    ("<*>" . "⊛")
    ("<|>" . "⟐")
    ("<$>" . "↥")
    ("*>" . "⩺")
    ("<*" . "⩹"))
  "Symbol prettification alist for `haskell-mode'. This will be
  appended to the default list provided by `haskell-mode'.")

(defun adq/haskell-setup ()
  "Defaults for Haskell."
  (setq haskell-interactive-popup-errors nil
        haskell-font-lock-symbols t)
  (intero-mode))

(adq/region-switch-command adq/hindent-format-region-or-buffer
  #'hindent-reformat-region #'hindent-reformat-buffer
  "Use hindent to format region or buffer.")

(defun adq/helm-stack-hoogle-transformer (candidates _source)
  (cl-loop for candidate in candidates
           when (stringp candidate)
           collect
           (let ((parts (s-split-up-to " -- " candidate 2)))
             (if (cadr parts)
                 (cons (car parts) (cadr parts))
               (car parts)))))

(defvar adq/helm-source-stack-hoogle
  (helm-build-async-source "Hoogle"
    :candidates-process
    (lambda ()
      (start-process "Hoogle" nil
                     "stack"
                     "hoogle"
                     "--no-setup"
                     "--"
                     "search"
                     "--count=512"
                     "--link"
                     helm-pattern))
    :requires-pattern 3
    :action '(("Browse documentation" . browse-url))
    :filtered-candidate-transformer #'adq/helm-stack-hoogle-transformer)
  "Source for searching Stack managed Hoogle.")

(defun adq/helm-stack-hoogle (d)
  "Search Stack managed Hoogle."
  (interactive "P")
  (helm :sources #'adq/helm-source-stack-hoogle
        :prompt "Hoogle: "
        :input (when-let (symbol (and d (symbol-at-point)))
                 (symbol-name symbol))
        :buffer "*Hoogle search*"))

;; FIXME: All the commands might not be present
(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.ghci\\'" . ghci-script-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (setq haskell-font-lock-symbols t)
  (setq haskell-font-lock-symbols-alist
        (append haskell-font-lock-symbols-alist adq/haskell-extra-font-lock-symbols-alist))
  (add-hook 'haskell-mode-hook  #'adq/haskell-setup)
  :bind
  (:map haskell-mode-map
        ("C-c a =" . adq/hindent-format-region-or-buffer)
        ("C-c a r" . hlint-refactor-refactor-at-point)
        ("C-c a R" . hlint-refactor-refactor-buffer)
        ("C-c a a" . hasky-stack-execute)
        ("C-c a c" . hasky-stack-package-action-popup)
        ("C-c a s" . haskell-sort-imports)
        ("C-c a e" . intero-restart)
        ("C-c a p" . intero-list-buffers)
        ("C-c a h" . adq/helm-stack-hoogle)
        ("C-c a H" . haskell-hoogle)

        ("C-c d d" . intero-goto-definition)))

(provide 'lang-haskell)

;;; lang-haskell.el ends here
