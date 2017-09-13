;;; init-dashboard.el --- Dashboard setup -*- lexical-binding: t -*-

(defvar dashboard-user-banner-directory
  (f-join init-directory "resources/banners")
  "Location for user's dashboard banners.")

(defun dashboard-select-banner ()
  "Return random file from user's banner directory of 'official."
  (-if-let (files
            (and (f-directory? dashboard-user-banner-directory)
                 (f--files dashboard-user-banner-directory
                           (equal (f-ext it) "png"))))
      (list-random-item files)
    'official))

(defun dashboard-setup ()
  "Setup dashboard."
  (setq-local mode-line-format nil))

(use-package dashboard
  :ensure t
  :init
  (add-hook 'dashboard-mode-hook #'dashboard-setup)
  :config
  (setq dashboard-items '((recents . 10)
                          (projects . 5))
        dashboard-startup-banner (dashboard-select-banner)
        dashboard-banner-logo-title
        (format "Welcome to Emacs, %s %s"
                (car (s-split " " (user-full-name)))
                (list-random-item '("🌈" "💖" "🌻" "🌸"))))
  (dashboard-setup-startup-hook))


(provide 'init-dashboard)
