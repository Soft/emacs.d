;;; init-session.el --- Session management -*- lexical-binding: t -*-

;;; Commentary:

;; Try to keep as much state between Emacs restarts as possible.

;;; Code:

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package saveplace
  :init (save-place-mode))

;; FIXME: I don't think this saves additional variables properly.
;; I should consider looking into session mode.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (add-to-list-many
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
     shell-command-history)))

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
        desktop-restore-eager 0
        desktop-auto-save-timeout 60)
  (desktop-save-mode 1))

(provide 'init-session)

;;; init-session.el ends here
