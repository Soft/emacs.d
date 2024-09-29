;;; config-eglot.el -*- lexical-binding: t; -*-

(defun adq/eglot-wanted-p (mode)
  "Returns t if eglot should be enabled for MODE."
  (pcase mode
    ((or 'c-mode 'c++-mode) (adq/programs-p "clangd"))
    ('go-mode (adq/programs-p "gopls"))
    ('rust-mode (adq/programs-p "rust-analyzer"))
    ('haskell-mode (adq/programs-p "haskell-language-server"))
    ('python-mode (adq/programs-p "pyright-langserver"
                                  "pyls"))
    ;; ('typescript-mode (adq/programs-p "typescript-language-server"))

    ))

(defun adq/maybe-enable-eglot ()
  "Try to enable eglot mode. Language server support will be
enabled if the buffer is part of a project and
`adq/eglot-wanted-p' returns true for the mode."
  (interactive)
  (when (and (adq/projectile-buffer-project)
             (adq/eglot-wanted-p major-mode))
    (eglot-ensure)))

(use-package eglot
  :defer t
  :straight nil
  :init
  (add-hook 'prog-mode-hook #'adq/maybe-enable-eglot)
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-mode) "typescript-language-server" "--stdio"))
  )

(provide 'config-eglot)
