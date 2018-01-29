;;; init-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Use telephone-line for pretty modeline

;;; Code:

(use-package telephone-line-config
  :ensure telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
        telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
        telephone-line-primary-right-separator 'telephone-line-tan-right
        telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right
        telephone-line-evil-use-short-tag t)
  (adq/after-load 'evil
    (telephone-line-evil-config)))

(provide 'init-modeline)

;;; init-modeline.el ends here
