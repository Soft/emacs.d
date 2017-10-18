;;; all-monitors.el --- Open frames on all monitors -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2") (dash))

;;; Commentary:

;; Fill all the monitors with Emacs frames. This does not work on all window
;; managers (for example, i3 does not support this), but for example on KWin
;; this seems to work just fine.

;;; Code:

(require 'dash)

(defun all-monitors-monitor-attributes (name)
  (seq-find
   (lambda (attributes)
     (equal  (cdr (assoc 'name attributes)) name))
   (display-monitor-attributes-list)))

(defun all-monitors-make-frame-on-monitor (name)
  (let ((attributes
         (cdr (assoc 'workarea (all-monitors-monitor-attributes name))))
        (frame (make-frame)))
    (modify-frame-parameters
     frame
     `((left . (+,(car attributes)))
       (top . (+ ,(cadr attributes)))
       (user-position . t)))
    frame))

(defun all-monitors-monitors-without-frames ()
  (--map (cdr (assoc 'name it))
         (--remove (cdr (assoc 'frames it))
                   (display-monitor-attributes-list))))

;;;###autoload
(defun all-monitors-frames-on-all-monitors (&optional state)
  "Create frame on all monitors where there currently isn't a
frame. The STATE can be ommited or it can be 'fullscreen or
'maximized. With 'fullscreen the new frames are set to be in the
fullscreen mode and with 'maximized the new frames are simply
maximized. If the STATE is ommited, the new frames are left with
their default appearance."
  (-each
      (-map #'all-monitors-make-frame-on-monitor
            (all-monitors-monitors-without-frames))
    (lambda (frame)
      (pcase state
        ('fullscreen (set-frame-parameter frame 'fullscreen 'fullboth))
        ('maximize (set-frame-parameter frame 'fullscreen 'maximied))))))

;;;###autoload
(defun all-monitors-fill-all-monitors (d)
  "Fill all monitors with frames. By default, the frames are set
to be in fullscreen mode but if universal argument is supplied,
the frames are simply maximized."
  (interactive "P")
  (all-monitors-frames-on-all-monitors (if d 'maximized 'fullscreen)))

;;; all-monitors.el ends here
