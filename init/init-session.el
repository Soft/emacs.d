;;; init-session.el --- Session management -*- mode: lexical-binding: t -*-

;; TODO: Add session mode

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package saveplace
  :init (save-place-mode))

;; savehist mode
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
  (setq recentf-max-saved-items 1000)
  (add-to-list-many
   'recentf-exclude
   '("/tmp/" "/ssh:" "/usr/share/emacs/" "\\.emacs\\.d/elpa/")))

;; Maybe I should reconsider this since it is so slow
(use-package desktop
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-base-file-name "desktop"
        desktop-restore-frames nil
        desktop-restore-eager 3
        desktop-auto-save-timeout 60)
  (desktop-save-mode 1))

(provide 'init-session)

