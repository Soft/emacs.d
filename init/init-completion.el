;;; init-completion.el --- Code completion -*- lexical-binding: t -*-

(use-package company
  :ensure t
  :defer t
  :diminish company-mode)

(use-package company-quickhelp
  :ensure t
  :defer t)

(use-package company-statistics
  :ensure t
  :defer t)

(provide 'init-completion)
