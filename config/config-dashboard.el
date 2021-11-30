;;; config-dashboard.el -*- lexical-binding: t; -*-

(defvar adq/dashboard-user-banner-directory
  (concat adq/adequate-directory "resources/banners")
  "Location for user's dashboard banners.")

(defun adq/dashboard-select-banner ()
  "Return random png file from `adq/dashboard-user-banner-directory'
or 'official if the directory is empty."
  (if-let (banners
           (and
            (file-directory-p adq/dashboard-user-banner-directory)
            (directory-files adq/dashboard-user-banner-directory t "\\.png\\'")))
      (seq-random-elt banners)
    'official))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)
                          (projects . 5))
        dashboard-footer-messages '("Happy coding!")
        dashboard-startup-banner (adq/dashboard-select-banner)))

(provide 'config-dashboard)