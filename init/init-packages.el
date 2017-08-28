;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Package management

(use-package paradox
  :ensure t
  :defer t
  :commands (paradox-enable))

(use-package package-utils
  :defer t
  :ensure t)

(run-with-idle-timer 1 nil (lambda () (paradox-enable)))

(defvar package-archive-old-seconds (* 120 60))

(defun package-should-refresh-p ()
  "Should package archive be refreshed."
  (if package-last-refresh-time
      (if  (time-less-p
            (time-subtract (current-time) package-last-refresh-time)
            (seconds-to-time package-archive-old-seconds))
          nil
        t) 
    t))

(defun install-packages-if-missing (packages &optional refresh)
  "Install PACKAGES if they are not already installed. If REFRESH is non-nil, refresh packages before installing if package-should-refresh-p returns non-nil."
  (let ((to-install (-remove #'package-installed-p packages)))
    (when to-install
      (when (and refresh (package-should-refresh-p))
        (package-refresh-contents))
      (-each to-install #'package-install))))

(provide 'init-packages)
