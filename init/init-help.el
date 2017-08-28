;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Help related functions

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package help-fns+
  :ensure t)

(defconst adequate-url "https://bitbucket.org/Soft/emacs.d/src")

(defun about-adequate-emacs-d ()
  (interactive)
  (browse-url adequate-url))

(define-key-after
  (lookup-key global-map [menu-bar help-menu])
  [adequate-website]
  '("About Adequate emacs.d" . about-adequate-emacs-d)
  'about-gnu-project)

(use-package keyfreq
  :ensure t
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(provide 'init-help)

