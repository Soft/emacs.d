;;; dbus-control.el --- Control Emacs using D-Bus -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2"))

;;; Commentary:

;; Minor mode for controlling Emacs using D-Bus method calls.

;;; Code:

(require 'dbus)

(defvar dbus-control--open-file-handle nil)
(defvar dbus-control--focus-handle nil)
(defvar dbus-control--exit-handle nil)

(defun dbus-control-open-file (path)
  (if (find-file path)
      '(:boolean t)
    '(:boolean nil)))

(defun dbus-control-focus ()
  (x-focus-frame nil)
  '(:boolean t))

(defun dbus-control-exit ()
  (run-at-time 1 nil #'kill-emacs)
  '(:boolean t))

;;;###autoload
(define-minor-mode dbus-control-mode
  "D-Bus control mode"
  :global t
  (if dbus-control-mode
      (progn
        (dbus-register-service :session dbus-service-emacs :replace-existing)
        (setq dbus-control--open-file-handle
              (dbus-register-method
               :session
               dbus-service-emacs dbus-path-emacs dbus-interface-emacs "OpenFile"
               #'dbus-control-open-file t))
        (setq dbus-control--focus-handle
              (dbus-register-method
               :session
               dbus-service-emacs dbus-path-emacs dbus-interface-emacs "Focus"
               #'dbus-control-focus t))
        (setq dbus-control--exit-handle
              (dbus-register-method
               :session
               dbus-service-emacs dbus-path-emacs dbus-interface-emacs "Exit"
               #'dbus-control-exit t)))
    (progn
      (dbus-unregister-object dbus-control--open-file-handle)
      (dbus-unregister-object dbus-control--focus-handle)
      (dbus-unregister-object dbus-control--exit-handle)
      (dbus-unregister-service :session dbus-service-emacs))))

(provide 'dbus-control)

;;; dbus-control.el ends here
