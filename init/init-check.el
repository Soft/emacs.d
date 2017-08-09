;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Syntax checking

(use-package flycheck
  :defer t
  :ensure t)

(use-package flyspell-lazy
  :defer t
  :ensure t)

(run-with-idle-timer
 1 nil
 (lambda ()
   (global-flycheck-mode)
   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(provide 'init-check)
