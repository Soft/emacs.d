;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Session management

(use-package restart-emacs
  :ensure t
  :defer t)

(save-place-mode)

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

(provide 'init-session)
