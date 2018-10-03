;;; screenshot.el --- Take screenshots  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (magit-popup))

;;; Commentary:

;; Utilities for taking screenshots

;;; Code:

(require 'magit-popup)

(defun screenshot-exec-scrot (file &optional args)
  (make-process
   :name "scrot"
   :command (append (cons "scrot"
                          (apply #'append (mapcar #'split-string args)))
                    (list file))
   :sentinel
   (lambda (process _event)
     (when (eq (process-status process) 'exit)
       (if (eq (process-exit-status process) 0)
           (message "Screenshot saved %s" file)
         (error "Taking screenshot failed"))))))

(defun screenshot-full-screen (file &optional args)
  (interactive
   (list (read-file-name "Output file: " nil nil nil nil)
         (screenshot-arguments)))
  (screenshot-exec-scrot file args))

(defun screenshot-select-window (file &optional args)
  (interactive (list (read-file-name "Output file: " nil nil nil nil)
                     (screenshot-arguments)))
  (screenshot-exec-scrot file (cons "--select" args)))

(defun screenshot-focused-window (file &optional args)
  (interactive (list (read-file-name "Output file: " nil nil nil nil)
                     (screenshot-arguments)))
  (screenshot-exec-scrot file (cons "--focused" args)))

;;;###autoload
(magit-define-popup screenshot
  "Popup for screenshots."
  :man-page "scrot"
  :switches '((?b "Border" "--border")
              (?m "Multiple Displays" "--multidisp"))
  :options '((?d "Delay" "--delay " read-number)
             (?q "Quality" "--quality " read-number)
             (?e "Execute" "--exec ")
             (?t "Thumbnail" "--thumb " read-number))
  :actions '((?c "Fullscreen" screenshot-full-screen)
             (?s "Selection" screenshot-select-window)
             (?f "Focused" screenshot-focused-window)))

(provide 'screenshot)
;;; screenshot.el ends here
