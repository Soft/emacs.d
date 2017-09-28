;;; init-help.el --- Help related functions -*- lexical-binding: t -*-

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-function)
   ("C-h v" . helpful-variable)
   ("C-h c" . helpful-command)
   ("C-h SPC" . helpful-at-point))
  :config
  (bind-keys
   :map helpful-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . scroll-down)
   ("K" . scroll-up)))

(bind-key "C-h K" #'describe-personal-keybindings)

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
  (setq keyfreq-file (f-join user-emacs-directory "keyfreq")
        keyfreq-file-lock (f-join user-emacs-directory "keyfreq.lock"))
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(provide 'init-help)

