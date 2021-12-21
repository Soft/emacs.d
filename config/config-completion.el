;;; config-completion.el -*- lexical-binding: t; -*-

(defvar adq/completion-system 'vertico
  "Completion system to use.")

(use-package company
  :hook (prog-mode . company-mode)
  :diminish company-mode
  :config
  (setq-default
   company-backends '(company-capf company-files)))

(cond
 ((eq adq/completion-system 'vertico)
  (require 'config-vertico))
 ((eq adq/completion-system 'helm)
  (require 'config-helm)))

(provide 'config-completion)
