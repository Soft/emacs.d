;;; init-libraries.el --- General purpose libraries -*- lexical-binding: t -*-

;; A modern list library for Emacs
(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

;; The long lost Emacs string manipulation library
(use-package s
  :ensure t)

;; Modern API for working with files and directories in Emacs
(use-package f
  :ensure t)

(use-package hydra
  :ensure t)

(use-package cl-lib)

(use-package fringe-helper
  :ensure t
  :functions (fringe-helper-define))

(provide 'init-libraries)

