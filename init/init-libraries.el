;;; init-libraries.el --- General purpose libraries -*- lexical-binding: t -*-

;;; Commentary:

;; Require libraries that are used by the rest of the configuration. This should
;; happen as early as possible.

;;; Code:

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

(use-package fringe-helper
  :ensure t
  :functions (fringe-helper-define))

(use-package cl-lib)

(use-package color
  :functions (color-distance
              color-lighten-name
              color-darken-name))

(provide 'init-libraries)

;;; init-libraries.el ends here
