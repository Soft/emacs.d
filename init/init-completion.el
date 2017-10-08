;;; init-completion.el --- Code completion -*- lexical-binding: t -*-

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (bind-keys
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(use-package company-quickhelp
  :ensure t
  :defer t)

(use-package company-statistics
  :ensure t
  :defer t)

(provide 'init-completion)
