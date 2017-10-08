;;; init-completion.el --- Code completion -*- lexical-binding: t -*-

;;; Commentary:

;; Use Company for code completion. This needs more work.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
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

;;; init-completion.el ends here
