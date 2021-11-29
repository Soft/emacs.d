;;; config-lsp.el -*- lexical-binding: t; -*-

(defvar adq/lsp-enabled-modes
  (append
   (when (adq/programs-p "clangd")
     '(c-mode c++-mode))
   (when (adq/programs-p "gopls")
     '(go-mode))
   (when (adq/programs-p "rust-analyzer")
     '(rust-mode))
   (when (adq/programs-p "haskell-language-server")
     '(haskell-mode literate-haskell-mode))
   (when (adq/programs-p "pyls")
     '(python-mode)))
  "List of modes where LSP should be enabled.")

(defun adq/maybe-enable-lsp ()
  "Try to enable LSP support. Language server support will be
enabled if the buffer is part of a project and its major mode is
in `adq/lsp-enabled-modes'."
  (interactive)
  (when (and (adq/projectile-buffer-project)
             (seq-contains-p adq/lsp-enabled-modes major-mode))
    (lsp-deferred)))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'prog-mode-hook #'adq/maybe-enable-lsp)
  :config
  (setq
   lsp-keep-workspace-alive nil
   lsp-enable-snippet nil)
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t)))

(use-package helm-lsp
  :after lsp-mode
  :config
  (define-key lsp-mode-map
    [remap xref-find-apropos]
    #'helm-lsp-workspace-symbol))

(provide 'config-lsp)
