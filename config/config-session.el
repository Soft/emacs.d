;;; config-session.el -*- lexical-binding: t; -*-

(use-package restart-emacs
  :defer t)

(use-package recentf
  :defer 1
  :config
  (setq recentf-max-saved-items 1024
        recentf-auto-cleanup 'never)
  (adq/add-many-to-list
   'recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode))

(provide 'config-session)
