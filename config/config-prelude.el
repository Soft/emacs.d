;;; config-prelude.el -*- lexical-binding: t; -*-

(defun adq/buffers-with-major-mode (mode)
  "Get a list of buffer with MODE."
  (seq-filter
   (lambda (buffer)
     (eq mode (buffer-local-value 'major-mode buffer)))
   (buffer-list)))

(defun adq/add-many-to-list (list elements &optional append compare-fn)
  "Add ELEMENTS to LIST using add-to-list."
  (dolist (item elements)
    (add-to-list list item append compare-fn)))

(defun adq/repeating (key fn &rest args)
  "Makes a function that can be repeated with additional key presses."
  (let ((map (make-sparse-keymap)))
    (cl-labels ((action ()
                        (interactive)
                        (apply fn args)
                        (set-transient-map map)))
      (define-key map (kbd key) #'action)
      #'action)))

(defun adq/insert-date (d)
  "Insert current date into the buffer. If invoked with the
universal argument present, current time will also be inserted."
  (interactive "P")
  (insert
   (if d
       (let ((tz (format-time-string "%z")))
         (concat
          (format-time-string "%Y-%m-%dT%T")
          (substring tz 0 3)
          ":"
          (substring tz 3 5)))
     (format-time-string "%Y-%m-%d"))))

(defvar adq/programs-p-cache (make-hash-table :test 'equal))

(defun adq/programs-p-clear-cache ()
  "Clear the internal hash table used by `adq/programs-p'."
  (interactive)
  (setq adq/programs-p-cache (make-hash-table :test 'equal)))

(defun adq/programs-p (&rest programs)
  "Returns t if any of the executables specified in arguments are
present. If the first argument is :all, all the programs must be
present."
  (pcase-let ((`(,op . ,progs)
               (cond ((eq (car programs) :all)
                      (cons #'cl-every (cdr programs)))
                     ((eq (car programs) :any)
                      (cons #'cl-some (cdr programs)))
                     (t
                      (cons #'cl-some programs)))))
    (funcall
     op
     #'identity
     (mapcar
      (lambda (prog)
        (let ((cached (gethash prog adq/programs-p-cache 'not-present)))
          (if (eq cached 'not-present)
              (let ((value (and (executable-find prog) t)))
                (puthash prog value adq/programs-p-cache)
                value)
            cached)))
      progs))))

(provide 'config-prelude)
