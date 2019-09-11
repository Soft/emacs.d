;;; ff-screenshot.el --- Take screenshots with Firefox -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Take screenshots of web pages with Firefox

;;; Code:

(defvar ff-screenshot-command "firefox"
  "Command to use to invoke Firefox.")

(defvar ff-screenshot-profile "ff-screenshot"
  "Firefox profile name to use for taking screenshots.")

(defvar ff-screenshot-default-size '(1000 . 800)
  "Default screenshot size.")

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
(defun ff-screenshot-capture (url path done-cb &optional size)
  "Take screenshot of URL and save it to PATH. Call done-cb if
taking screenshot succeeds.

Size can have one of the followign forms:
- (WIDTH . HEIGHT)
- WIDTH
- nil"
  (let ((size (pcase size
                (`(,width . ,height) (format "%d,%d" width height))
                ((pred integerp) (format "%d" size))
                (`nil nil)
                (_ (error "Invalid size")))))
    (ff-screenshot-exec
     done-cb
     `("--headless"
       "--no-remote"
       "-P" ,ff-screenshot-profile
       "--screenshot" ,path
       ,@(when size
           `("--window-size" ,size))
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
       (find-file path))
     ff-screenshot-default-size)))

(provide 'ff-screenshot)

;;; ff-screenshot.el ends here
