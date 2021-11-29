;;; config-libraries.el -*- lexical-binding: t; -*-

;; Emacs bindings that stick around
(use-package hydra :defer t)

;; Collection of icons
(use-package all-the-icons :defer t)

;; Library for defining fringe bitmaps
(use-package fringe-helper
  :functions fringe-helper-define)

(provide 'config-libraries)
