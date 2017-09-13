;;; init-scratch.el --- Scratch -*- lexical-binding: t -*-

;; I didn't really use scratch buffer for emacs lisp so having it be a general
;; purpose notepad is more useful for me
(setq initial-major-mode 'org-mode
      initial-scratch-message
      (concat
       (replace-regexp-in-string "^" "# " (emacs-version))
       "\n\n"))

;; This could just as well be in init-session.el
(use-package persistent-scratch
  :ensure t
  :init
  (setq persistent-scratch-save-file
        (f-join user-emacs-directory "scratch.org"))
  (persistent-scratch-setup-default))

(provide 'init-scratch)
