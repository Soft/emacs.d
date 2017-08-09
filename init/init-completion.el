;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Code completion

(use-package company
  :ensure t
  :defer t)

(use-package company-quickhelp
  :ensure t
  :defer t)

(use-package company-statistics
  :ensure t
  :defer t)

(provide 'init-completion)
