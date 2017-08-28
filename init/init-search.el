;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Search configuration

(use-package pcre2el
  :ensure t
  :diminish pcre-mode
  :init (pcre-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode))

;; FIXME: This really needs more work
(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode)

(use-package dumb-jump
  :ensure t
  ;;:config (setq dumb-jump-selector 'helm)
  :bind (("C-c d d" . dumb-jump-go)
         ("C-c d b" . dumb-jump-back)
         ("C-c d q" . dumb-jump-quick-look)))

(provide 'init-search)

