#!/usr/bin/emacs --script

;;; Commentary

;; Open files in existing Emacs instance using the DBUS interface provided by
;; dbus-control.el or, alternatively, spawn a new emacs instance

;;; Code:

(require 'dbus)

(defconst DBUS-TIMEOUT 500)

(defun open-files (paths)
  "Open PATHS in Emacs."
  (mapc
   (lambda (path)
     (dbus-call-method
      :session dbus-service-emacs dbus-path-emacs dbus-interface-emacs
      "OpenFile" :timeout DBUS-TIMEOUT path))
   paths))

(defun focus ()
  "Focus Emacs window."
  (dbus-call-method
   :session dbus-service-emacs dbus-path-emacs dbus-interface-emacs
   "Focus" :timeout DBUS-TIMEOUT))

(let ((paths (mapcar #'expand-file-name argv)))
  (if (member dbus-service-emacs (dbus-list-known-names :session))
      (progn
        (open-files paths)
        (focus))
    (apply #'call-process (append (list (executable-find "emacs") nil 0 nil) paths))))

;; emacs-opener.el ends here
