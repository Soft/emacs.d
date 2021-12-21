;;; config-completion.el -*- lexical-binding: t; -*-

(defcustom adq/completion-system 'vertico
  "Completion system to use."
  :group 'adequate
  :type '(choice (const :tag "Vertico" vertico)
                 (const :tag "Helm" helm)))

(cond
 ((eq adq/completion-system 'vertico)
  (require 'config-vertico))
 ((eq adq/completion-system 'helm)
  (require 'config-helm)))

(use-package company
  :hook (prog-mode . company-mode)
  :diminish company-mode
  :config
  (setq-default
   company-backends '(company-capf company-files)))

(provide 'config-completion)
