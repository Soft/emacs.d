;;; config-completion.el -*- lexical-binding: t; -*-

(use-package company
  :hook (prog-mode . company-mode)
  :diminish company-mode
  :config
  (setq-default
   company-backends '(company-capf company-files)))

(provide 'config-completion)
