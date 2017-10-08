;;; lang-org.el --- Org mode -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode configuration

;;; Code:

(defun org-setup ()
  "Setup org-mode"
  (org-bullets-mode))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-setup))

(use-package org-bullets
  :ensure t
  :defer t)

(provide 'lang-org)

;;; lang-org.el ends here
