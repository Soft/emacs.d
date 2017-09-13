;;; init-session.el --- Session management -*- mode: lexical-binding: t -*-

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package saveplace
  :init (save-place-mode))

(use-package savehist
  :init (savehist-mode)
  :config
  (add-to-list-many
   'savehist-additional-variables
   '(search-ring
     kill-ring
     set-variable-value-history
     shell-command-history
     file-name-history
     regexp-search-ring)))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200))

;; Maybe I should reconsider this since it is so slow
(use-package desktop
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-base-file-name "desktop"
        desktop-auto-save-timeout 60)
  (desktop-save-mode 1))

(provide 'init-session)
