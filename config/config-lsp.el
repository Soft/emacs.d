;;; config-lsp.el -*- lexical-binding: t; -*-

(defun adq/lsp-wanted-p (mode)
  "Returns t if LSP should be enabled for MODE."
  (pcase mode
    ((or 'c-mode 'c++-mode) (adq/programs-p "clangd"))
    ('go-mode (adq/programs-p "gopls"))
    ('rust-mode (adq/programs-p "rust-analyzer"))
    ('haskell-mode (adq/programs-p "haskell-language-server"))
    ('python-mode (adq/programs-p "pyls"))))

(defun adq/maybe-enable-lsp ()
  "Try to enable LSP support. Language server support will be
enabled if the buffer is part of a project and its major mode is
in `adq/lsp-enabled-modes'."
  (interactive)
  (when (and (adq/projectile-buffer-project)
             (adq/lsp-wanted-p major-mode))
    (lsp-deferred)))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'prog-mode-hook #'adq/maybe-enable-lsp)
  :config
  (setq
   lsp-file-watch-threshold nil
   lsp-keep-workspace-alive nil
   lsp-enable-snippet nil)
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t)))

(use-package helm-lsp
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ("<remap> <xref-find-apropos>" .
         (lambda ()
           (interactive) ;; Use symbol at point by default
           (helm-lsp-workspace-symbol t)))))

(provide 'config-lsp)
