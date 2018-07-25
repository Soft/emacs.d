;;; init-session.el --- Session management -*- lexical-binding: t -*-

;;; Commentary:

;; Try to keep as much state between Emacs restarts as possible.

;;; Code:

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package saveplace
  :init
  (setq save-place-forget-unreadable-files nil)
  (save-place-mode))

;; FIXME: I don't think this saves additional variables properly.
;; I should consider looking into session mode.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (adq/add-to-list-many
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

;; (use-package session
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq session-save-file (f-join adq/init-directory "session")
;;         desktop-globals-to-save
;;         (append '((comint-input-ring  . 100)
;;                   (compile-history    . 50)
;;                   (minibuffer-history . 100))
;;                 desktop-globals-to-save))
;;   (add-hook 'after-init-hook #'session-initialize))

(use-package recentf
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 1000)
  (adq/add-to-list-many
   'recentf-exclude
   '("/tmp/"
     "/ssh:"
     "/usr/share/emacs/"
     "\\.emacs\\.d/elpa/"
     "TAGS"
     "\\.stack-work/")))

;; Maybe I should reconsider this since it is so slow
(use-package desktop
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-dirname user-emacs-directory
        desktop-base-file-name "desktop"
        desktop-save t
        desktop-restore-frames nil
        desktop-restore-eager 0
        desktop-auto-save-timeout 60)
  (desktop-save-mode 1)
  :config
  (add-to-list 'desktop-clear-preserve-buffers "\\*dashboard\\*"))

(use-package midnight
  :diminish midnight-mode
  :bind (("C-c x C" . clean-buffer-list))
  :config
  (add-to-list 'clean-buffer-list-kill-regexps
               "*magit: ")
  (add-to-list 'clean-buffer-list-kill-never-buffer-names
               "*dashboard*"))

(provide 'init-session)

;;; init-session.el ends here
