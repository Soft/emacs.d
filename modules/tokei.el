;; tokei.el --- Code Statistics  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Display code statistics using tokei.

;;; Code:

(require 'tabulated-list)

(defvar tokei-command "tokei"
  "Tokei command.")

(defvar-local tokei-paths '())

(defun tokei-exec (paths callback &rest args)
  "Execute tokei at `path` calling callback with the result."
  (let* ((root (string-join paths " "))
         (buffer (generate-new-buffer (format "*execute: tokei %s*" root))))
    (make-process
     :name (format "tokei %s" root)
     :buffer buffer
     :noquery t
     :command (append (list tokei-command
                            "--output" "json" "--")
                      paths)
     :sentinel
     (lambda (process _event)
       (unwind-protect
           (when (eq (process-status process) 'exit)
             (pcase (process-exit-status process)
               (`0 (with-current-buffer buffer
                     (goto-char (point-min))
                     (apply callback
                            (cons
                             (let ((json-key-type 'string)
                                   (json-array-type 'list))
                               (json-read))
                             args))))
               (n (signal 'tokei-failed
                          (format "Tokei returned an error (exit code: %d)" n)))))
         (kill-buffer buffer))))))

(defun tokei-mode-refresh ()
  "Refresh `tokei-mode' buffer contents."
  (tokei-exec
   tokei-paths
   (lambda (statistics buffer)
     (with-current-buffer buffer
       (setq tabulated-list-entries
             (cl-loop for (lang . data) in statistics
                      collect
                      (list
                       lang
                       (vector
                        lang
                        (number-to-string
                         (length (cdr (assoc "stats" data))))
                        (number-to-string
                         (cdr (assoc "lines" data)))
                        (number-to-string
                         (cdr (assoc "code" data)))
                        (number-to-string
                         (cdr (assoc "comments" data)))
                        (number-to-string
                         (cdr (assoc "blanks" data)))))))
       (tabulated-list-print t)))
   (current-buffer)))

(define-derived-mode tokei-mode tabulated-list-mode "Tokei"
  "Major mode for displaying Tokei statistics."
  (setq tabulated-list-format
        '[("Language" 30)
          ("Files" 10 nil :right-align t)
          ("Lines" 10 nil :right-align t)
          ("Code" 10 nil :right-align t)
          ("Comments" 10 nil :right-align t)
          ("Blanks" 10 nil :right-align t)])
  (tabulated-list-init-header)
  (tokei-mode-refresh))

;;;###autoload
(defun tokei ()
  "Display code statistics about current project."
  (interactive)
  (let* ((roots (project-roots (project-current t)))
         (buffer (get-buffer-create
                  (format "*tokei %s*"
                          (string-join roots " ")))))
    (with-current-buffer buffer
      (setq-local tokei-paths roots)
      (if (eq major-mode 'tokei-mode)
          (tokei-mode-refresh)
        (tokei-mode)))
    (pop-to-buffer buffer)))

(provide 'tokei)
;;; tokei.el ends here
