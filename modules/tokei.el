;; tokei.el --- Display Source Code Statistics  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Display source code statistics using tokei.

;;; Code:

(require 'tabulated-list)

(defgroup tokei nil
  "Major mode for displaying source code statistics using Tokei."
  :group 'tools
  :prefix "tokei-")

(defvar tokei-command "tokei"
  "Tokei command.")

(defvar tokei-roots '())

(defvar tokei-paths '())

(defface tokei-totals
  '((t (:inherit bold)))
  "Face for totals."
  :group 'tokei)

(defvar tokei-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'tokei-mode-refresh)
    (define-key map (kbd "q") #'kill-buffer)
    map)
  "Keymap for `tokei-mode'.")

(defun tokei-supported-p ()
  "Returns t if tokei is available and supports JSON output."
  (and (executable-find tokei-command)
       (eq (call-process tokei-command nil nil nil "--output" "json" "/dev/null") 0)))

(defun tokei-exec (paths callback &rest args)
  "Execute tokei with `paths' calling `callback' with the result."
  (let* ((root (string-join paths " "))
         (buffer (generate-new-buffer (format "task: tokei %s*" root))))
    (make-process
     :name (format "task: tokei %s" root)
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

(defun tokei-make-entry (lang files lines code comments blanks &optional face)
  "Make tokei entry."
  (list lang
        (cl-map 'vector (if face
                            (lambda (str) (propertize str 'face face))
                          #'identity)
                (vector lang
                        (number-to-string files)
                        (number-to-string lines)
                        (number-to-string code)
                        (number-to-string comments)
                        (number-to-string blanks)))))

(defun tokei-mode-refresh ()
  "Refresh `tokei-mode' buffer contents."
  (interactive)
  (unless tokei-paths
    (error "`tokei-paths' is not set."))
  (tokei-exec
   tokei-paths
   (lambda (statistics buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq tabulated-list-entries
               (cl-loop for (lang . data) in statistics
                        for files = (length (cdr (assoc "stats" data)))
                        for lines = (cdr (assoc "lines" data))
                        for code = (cdr (assoc "code" data))
                        for comments = (cdr (assoc "comments" data))
                        for blanks = (cdr (assoc "blanks" data))
                        summing files into total-files
                        summing lines into total-lines
                        summing code into total-code
                        summing comments into total-comments
                        summing blanks into total-blanks
                        collecting
                        (list lang files lines code comments blanks)
                        into entries
                        finally return
                        (append
                         (mapcar (apply-partially #'apply #'tokei-make-entry)
                                 (cl-stable-sort entries #'> :key #'caddr)) 
                         (list (tokei-make-entry
                                "Total"
                                total-files
                                total-lines
                                total-code
                                total-comments
                                total-blanks
                                'tokei-totals)))))
         (tabulated-list-print t))))
   (current-buffer)))

(defun tokei-make-number-comparer (col)
  (lambda (a b)
    (> (string-to-number (elt (cadr a) col) 10)
       (string-to-number (elt (cadr b) col) 10))))

(defun tokei-make-string-comparer (col)
  (lambda (a b)
    (string-greaterp (elt (cadr a) col)
                     (elt (cadr b) col))))

(define-derived-mode tokei-mode tabulated-list-mode "Tokei"
  "Major mode for displaying source code statistics using Tokei.

\\{tokei-mode-map}"
  (setq tabulated-list-format
        `[("Language" 40 ,(tokei-make-string-comparer 0))
          ("Files" 10 ,(tokei-make-number-comparer 1) :right-align t)
          ("Lines" 10 ,(tokei-make-number-comparer 2) :right-align t)
          ("Code" 10 ,(tokei-make-number-comparer 3) :right-align t)
          ("Comments" 10 ,(tokei-make-number-comparer 4) :right-align t)
          ("Blanks" 10 ,(tokei-make-number-comparer 5) :right-align t)])
  (setq-local tokei-paths tokei-roots)
  (tabulated-list-init-header)
  (tokei-mode-refresh))

;;;###autoload
(defun tokei ()
  "Display code statistics about current project."
  (interactive)
  (unless (tokei-supported-p)
    (error "tokei is not available or does not seem to support JSON output format."))
  (let* ((roots (project-roots (project-current t)))
         (buffer (get-buffer-create
                  (format "*tokei %s*"
                          (string-join roots " ")))))
    (with-current-buffer buffer
      (unless (eq major-mode 'tokei-mode)
        (let ((tokei-roots roots))
          (tokei-mode)))
      (tokei-mode-refresh))
    (pop-to-buffer buffer)))

(provide 'tokei)
;;; tokei.el ends here
