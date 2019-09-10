;;; ff-screenshot.el --- Take screenshots with Firefox -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Take screenshots of web pages with Firefox

;;; Code:

(defvar ff-screenshot-command "firefox"
  "Command to use to invoke Firefox.")

(defvar ff-screenshot-profile "ff-screenshot"
  "Firefox profile name to use for taking screenshots.")

(defvar ff-screenshot-default-width 1000
  "Default screenshot width.")

(defvar ff-screenshot-default-height 800
  "Default screenshot height.")

(defun ff-screenshot-exec (done-cb &optional args)
  "Launch Firefox and call done-cb after process exits."
  (make-process
   :name "firefox"
   :command (cons ff-screenshot-command args)
   :sentinel
   (lambda (process _even)
     (when (eq (process-status process) 'exit)
       (pcase (process-exit-status process)
         (`0 (funcall done-cb))
         (n (error "Executing firefox failed (exit code: %d)" n)))))))

;;;###autoload
(defun ff-screenshot-capture (url path done-cb &optional width height)
  "Take screenshot of URL and save it to PATH. Call done-cb if
taking screenshot succeeds."
  (let ((width (or width ff-screenshot-default-width))
        (height (or height ff-screenshot-default-height)))
    (ff-screenshot-exec
     done-cb
     `("--headless"
       "--no-remote"
       "-P" ,ff-screenshot-profile
       "--screenshot" ,path
       "--window-size" ,(format "%d,%d" width height)
       ,url))))

;;;###autoload
(defun ff-screenshot-view (url)
  "Take screenshot of URL and open it for viewing."
  (interactive "sURL: ")
  (let ((path (make-temp-file "ff-screenshot-" nil ".png")))
    (ff-screenshot-capture
     url
     path
     (lambda ()
       (find-file path)))))

(provide 'ff-screenshot)

;;; ff-screenshot.el ends here
