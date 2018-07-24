;;; init-prelude.el --- Basic utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Useful utility functions.

;;; Code:

(defun adq/compose (fn &rest others)
  "Compose a set of functions."
  (let ((fns (cons fn others)))
    (lambda (&rest args)
      (cl-reduce (lambda (a b)
                   (funcall a b))
                 (butlast fns)
                 :from-end t
                 :initial-value (apply (car (last fns)) args)))))

(defmacro adq/partial (fn &rest template)
  "Partially apply function using a template."
  `(lambda (&rest args)
     (let ((merged-args (cl-loop for arg in (quote ,template)
                                 if (eq arg '_) collect (pop args)
                                 else collect (eval arg))))
       (apply (quote ,fn) merged-args))))

(defmacro adq/dlet* (varlist &rest body)
  "Destructuring let*."
  (declare (debug let)
           (indent 1))
  (cl-reduce (lambda (a b)
               `(cl-destructuring-bind ,(car a) ,(cadr a) ,b))
             varlist
             :from-end t
             :initial-value `(progn ,@body)))

(defmacro adq/if-supported (fn &rest args)
  "Call FN with ARGS if it is available."
  `(when (fboundp (quote ,fn)) (apply (quote ,fn) (quote ,args))))

;; Borrowed from https://github.com/jcf/emacs.d
(defmacro adq/after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro adq/numeric-argument-switch (form &rest others)
  "Makes a function that executes a FORM based on a numeric argument."
  (let* ((forms (cons form others))
         (conds (cl-loop for (i . f) in forms
                         collect `((equal d ,i) ,f))))
    `(lambda (d)
       (interactive "p")
       (cond ,@conds
             (t (error "No action bound to %d" d))))))

;; FIXME: This doesn't really work with interactive functions that take arguments
(defun adq/repeating (key fn &rest args)
  "Makes a function that can be repeated with additional key presses."
  (let ((map (make-sparse-keymap)))
    (cl-labels ((action ()
                        (interactive)
                        (apply fn args)
                        (set-transient-map map)))
      (define-key map (kbd key) #'action)
      #'action)))

(defmacro adq/switch-command (form &rest others)
  "Make a multi-purpose command that executes a FORM based on a numeric argument."
  (let* ((forms (cons form others))
         (nums (cl-loop for i from 1 to (1-  (length forms))
                        collect (expt 4 i)))
         (args (cl-mapcar #'cons (cons 1 nums) forms)))
    `(adq/numeric-argument-switch ,@args)))

(defun adq/switch-to-buffer-dwim (buffer)
  "Display BUFFER in the selected window or, if the buffer is
already visible in some window, switch focus to the window
containing it."
  (-if-let (window (get-buffer-window buffer))
      (select-window window)
    (switch-to-buffer buffer)))

(defun adq/switch-or-call (buffer-fn fn &rest args)
  "Use BUFFER-FN to retrieve a buffer and switch to it, or call FN with ARGS."
  (-if-let (buffer (funcall buffer-fn))
      (switch-to-buffer-other-window buffer)
    (apply fn args)))

(defun adq/buffers-with-major-mode (mode)
  "Get a list of buffer with MODE."
  (-filter
   (lambda (buffer) (with-current-buffer buffer (eq mode major-mode)))
   (buffer-list)))

(defun adq/list-random-item (list)
  "Return random item from LIST."
  (nth (random (length list)) list))

(defun adq/add-to-list-many (list to-add)
  "Add items from TO-ADD to LIST using add-to-list."
  (dolist (item to-add)
    (add-to-list list item)))

(defvar adq/programs-p-cache (make-hash-table :test 'equal))

(defun adq/programs-p-clear-cache ()
  "Clear the internal hash table used by `adq/programs-p'."
  (interactive)
  (setq adq/programs-p-cache (make-hash-table :test 'equal)))

(defun adq/programs-p (x &rest xs)
  "Returns t if any of the executables specified in arguments are
present. If the first argument is :all, all the programs must be
present."
  (let* (programs
         (fn (if (eq x :all)
                 (progn (setq programs xs)
                        #'-all?)
               (progn (setq programs (cons x xs))
                      #'-any?))))
    (funcall
     fn
     (lambda (prog)
       (let ((cached (gethash prog adq/programs-p-cache 'not-present)))
         (if (eq cached 'not-present)
             (let ((value (executable-find prog)))
               (puthash prog value adq/programs-p-cache)
               value)
           cached))) programs)))

(defun adq/adjust-hash (fn key table)
  "Look up KEY in TABLE and apply FN to the value and place the
transformed value back into TABLE. If KEY is not in TABLE, FN is
called without an argument to create the initial value."
  (let ((value (gethash key table)))
    (puthash key (funcall fn value) table))
  table)

(defun adq/clamp (min max value)
  "Clamp VALUE between MIN and MAX."
  (cond ((< value min) min)
        ((< value max) value)
        (t max)))

(defmacro adq/debug-message (message &rest args)
  "Similar to message but only produces output when `adq/emacs-debug'
is non-nil."
  (when adq/emacs-debug
    `(message ,message ,@args)))

(defmacro adq/region-switch-command (name with-region without-region &optional docstring)
  "Make a combination command that executes WITH-REGION if region
is active and WITHOUT-REGION if there is no active region."
  (declare (indent defun))
  `(defun ,name ()
     ,@(if docstring (list docstring) '())
     (interactive)
     (if (region-active-p)
         (call-interactively ,with-region)
       (call-interactively ,without-region))))

(defun adq/make-compiler (command name-transformer args-maker)
  "Create a new interactive command that receives the content of
the current buffer when executed."
  (let* ((command-base (file-name-base command))
         (process-name (concat command-base "-process"))
         (buffer-name (format "*%s-Log*" (capitalize command-base)))
         (args (list process-name buffer-name command)))
    (lambda ()
      (interactive)
      (let* ((name (file-name-base buffer-file-name))
             (dir (file-name-directory buffer-file-name))
             (output (funcall name-transformer
                              (concat (file-name-as-directory dir) name)))
             (proc (apply #'start-process
                          (cl-concatenate 'list args (funcall args-maker output)))))
        (message "%s: %s" (capitalize command-base) output)
        (process-send-string proc (buffer-string))
        (process-send-eof proc)))))

(defun adq/color-derive (change color)
  "Derive a new color based on COLOR that is either darker or
lighter than the original. If the original COLOR is closer to
black than white the returned color will be made lighter,
alternatively, if the original COLOR is closer the white than
black the returned color will be made darker. How much lighter or
darker the new color will be is determined by CHANGE."
  (if (> (color-distance color "#ffffff")
         (color-distance color "#000000"))
      (color-lighten-name color change)
    (color-darken-name color change)))

(defun adq/insert-date (d)
  "Insert current date into the buffer. If invoked with the
universal argument present, current time will also be inserted."
  (interactive "P")
  (insert
   (if d (format-time-string "%c")
     (format-time-string "%A %x"))))


(provide 'init-prelude)

;;; init-prelude.el ends here
