;; scrot.el --- Take screenshots using scrot  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (magit-popup))

;;; Commentary:

;; Utilities for taking screenshots using scrot

;;; Code:

(require 'magit-popup)

(defvar scrot-command "scrot"
  "Scrot command")

(defun scrot-exec (file &optional args)
  (make-process
   :name "scrot"
   :command (append
             (cons scrot-command
                   (apply #'append
                          (mapcar #'split-string-and-unquote args)))
             (list file))
   :sentinel
   (lambda (process _event)
     (when (eq (process-status process) 'exit)
       (if (eq (process-exit-status process) 0)
           (message "Screenshot saved %s" file)
         (error "Taking screenshot failed"))))))

(defun scrot-full-screen (file &optional args)
  "Take screenshot of the entire screen."
  (interactive
   (list (read-file-name "Output file: " nil nil nil nil)
         (scrot-arguments)))
  (scrot-exec file args))

(defun scrot-select-window (file &optional args)
  "Take screenshot of selected area."
  (interactive (list (read-file-name "Output file: " nil nil nil nil)
                     (scrot-arguments)))
  (scrot-exec file (cons "--select" args)))

(defun scrot-focused-window (file &optional args)
  "Take screenshot of focused window."
  (interactive (list (read-file-name "Output file: " nil nil nil nil)
                     (scrot-arguments)))
  (scrot-exec file (cons "--focused" args)))

;;;###autoload
(magit-define-popup scrot
  "Take screenshot using scrot."
  :man-page "scrot"
  :switches '((?b "Border" "--border")
              (?m "Multiple Displays" "--multidisp"))
  :options '((?d "Delay" "--delay " read-number)
             (?q "Quality" "--quality " read-number)
             (?e "Execute" "--exec ")
             (?t "Thumbnail" "--thumb " read-number))
  :actions '((?c "Fullscreen" scrot-full-screen)
             (?s "Selection" scrot-select-window)
             (?f "Focused" scrot-focused-window)))

(provide 'scrot)
;;; scrot.el ends here
