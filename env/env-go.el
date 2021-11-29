;;; env-go.el -*- lexical-binding: t; -*-

(defvar adq/go-prettify-symbols-alist
  '(("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("<-" . ?←)
    ("..." . ?…))
  "Symbol prettification alist for `go-mode'.")

(defun adq/go-setup ()
  "Defaults for Go programming language."
  (setq-local prettify-symbols-alist adq/go-prettify-symbols-alist)
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(define-derived-mode conf-go-mod-mode conf-mode "Go Module"
  "Mode for editing go.mod files."
  (conf-mode-initialize "//"))

(setq auto-mode-alist
      (append '(("/go\\.mod$" . conf-go-mod-mode))
              auto-mode-alist))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :init (add-hook 'go-mode-hook #'adq/go-setup))

(provide 'env-go)
