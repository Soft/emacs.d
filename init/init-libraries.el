
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

(use-package cl-lib)

(provide 'init-libraries)
