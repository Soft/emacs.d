;;; config-session.el -*- lexical-binding: t; -*-

(use-package restart-emacs
  :defer t)

(use-package recentf
  :straight nil
  :defer 1
  :config
  (setq recentf-max-saved-items 1024
        recentf-auto-cleanup 'never)
  (adq/add-many-to-list
   'recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode))

(use-package savehist
  :straight nil
  :defer 1
  :config
  (adq/add-many-to-list
   'savehist-additional-variables
   '(comint-input-ring
     compile-history
     file-name-history
     grep-find-history
     grep-history
     kill-ring
     query-replace-history
     read-expression-history
     regexp-history
     regexp-search-ring
     register-alist
     search-ring
     set-variable-value-history
     shell-command-history))
  (savehist-mode))

(provide 'config-session)
