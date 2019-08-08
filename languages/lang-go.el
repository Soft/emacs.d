;;; lang-go.el --- Go -*- lexical-binding: t -*-

;;; Commentary:

;; Go programming language

;;; Code:

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
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (adq/setq-local compilation-read-command nil
                  compile-command "go build"))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :ensure t
  :init (add-hook 'go-mode-hook #'adq/go-setup))

(provide 'lang-go)

;;; lang-go.el ends here
