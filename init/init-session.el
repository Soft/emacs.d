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

;; Protect certain buffers from being killed
(defvar immortal-buffers
  '("*scratch*" "*Messages*" "*dashboard*")
  "Buffers that cannot be killed.")

(defun kill-buffer-keep-immortal (buffer)
  "Protect immortal buffers from being killed."
  (interactive (list (current-buffer)))
  (let ((name (buffer-name buffer)))
    (if (member name immortal-buffers)
        (progn
          (message "%s is immortal and cannot be killed." name)
          (call-interactively #'bury-buffer buffer))
      (kill-buffer buffer))))

(global-set-key [remap kill-buffer] #'kill-buffer-keep-immortal)

(provide 'init-session)
