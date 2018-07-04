;;; init-dashboard.el --- Dashboard setup -*- lexical-binding: t -*-

;;; Commentary:

;; Fancy dashboard with random banner images. Place the images you want to use
;; inside Adequate's `resources/banners' directory.

;;; Code:

(defvar adq/dashboard-user-banner-directory
  (f-join adq/init-directory "resources/banners")
  "Location for user's dashboard banners.")

(defun adq/dashboard-select-banner ()
  "Return random file from user's banner directory or 'official."
  (-if-let (files
            (and (f-directory? adq/dashboard-user-banner-directory)
                 (f--files adq/dashboard-user-banner-directory
                           (equal (f-ext it) "png"))))
      (adq/list-random-item files)
    'official))

(defun adq/dashboard-setup ()
  "Setup dashboard."
  (setq-local mode-line-format nil))

(use-package dashboard
  :ensure t
  :init
  (add-hook 'dashboard-mode-hook #'adq/dashboard-setup)
  :config
  (setq dashboard-items '((recents . 10)
                          (projects . 5))
        dashboard-startup-banner (adq/dashboard-select-banner)
        dashboard-banner-logo-title
        (format "Welcome to Emacs, %s %s"
                (car (s-split " " (user-full-name)))
                (adq/list-random-item '("🌈" "💖" "🌻" "🌸"))))
  (bind-keys
   :map dashboard-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("J" . dashboard-next-section)
   ("K" . dashboard-previous-section))
  (advice-add #'dashboard-refresh-buffer
              :before (lambda ()
                        (setq dashboard-startup-banner (adq/dashboard-select-banner))))
  (dashboard-setup-startup-hook))


(provide 'init-dashboard)

;; init-dashboard.el ends here
