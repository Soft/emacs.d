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

;; Emacs bindings that stick around
(use-package hydra
  :ensure t)

;; Async helpers
(use-package async
  :ensure t)

;; Library for performingwith HTTP requets
(use-package request
  :ensure t
  :functions (request))

;; Library for defining fringe bitmaps
(use-package fringe-helper
  :ensure t
  :functions (fringe-helper-define)
  :defines (request-timeout
            request-backend
            request-curl
            request-curl-options
            request-log-level
            request-message-level
            request-storage-directory))

;; Common lisp compatability layer
(use-package cl-lib)

;; Functions for working with colors
(use-package color
  :functions (color-distance
              color-lighten-name
              color-darken-name))

(provide 'init-libraries)

;;; init-libraries.el ends here
