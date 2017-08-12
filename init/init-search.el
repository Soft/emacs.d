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

(provide 'init-search)

