;;; Syntax checking

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checks '(emacs-lisp-checkdoc)))

(use-package flyspell-lazy
  :ensure t)

(provide 'init-check)
