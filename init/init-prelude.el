;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Basic utilities 

(defun compose (fn &rest others)
  "Compose a set of functions."
  (let ((fns (cons fn others)))
    (lambda (&rest args)
      (cl-reduce (lambda (a b)
                   (funcall a b))
                 (butlast fns)
                 :from-end t
                 :initial-value (apply (car (last fns)) args)))))

(defmacro partial (fn &rest template)
  "Partially apply function using a template."
  `(lambda (&rest args)
     (let ((merged-args (cl-loop for arg in (quote ,template)
                                 if (eq arg '_) collect (pop args)
                                 else collect (eval arg))))
       (apply (quote ,fn) merged-args))))


(defmacro dlet* (varlist &rest body)
  "Destructuring let*."
  (declare (debug let)
           (indent 1))
  (cl-reduce (lambda (a b)
               `(cl-destructuring-bind ,(car a) ,(cadr a) ,b))
             varlist
             :from-end t
             :initial-value `(progn ,@body)))

(defmacro if-supported (fn &rest args)
  "Call FN with ARGS if it is available."
  `(when (fboundp (quote ,fn)) (apply (quote ,fn) ,args)))

;; Borrowed from https://github.com/jcf/emacs.d
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro numeric-argument-switch (form &rest others)
  "Makes a function that executes a FORM based on a numeric argument."
  (let* ((forms (cons form others))
         (conds (cl-loop for (i . f) in forms
                         collect `((equal d ,i) ,f))))
    `(lambda (d)
       (interactive "p")
       (cond ,@conds
             (t (error "No action bound to %d" d))))))

(defun repeating (key fn &rest args)
  "Makes a function that can be repeated with additional key presses."
  (let ((map (make-sparse-keymap)))
    (cl-labels ((action ()
                        (interactive)
                        (apply fn args)
                        (set-transient-map map)))
      (define-key map (kbd key) #'action)
      #'action)))

(defmacro switch-command (form &rest others)
  "Make a multi-purpose command that executes a FORM based on a numeric argument."
  (let* ((forms (cons form others))
         (nums (cl-loop for i from 1 to (1-  (length forms))
                        collect (expt 4 i)))
         (args (cl-mapcar #'cons (cons 1 nums) forms)))
    `(numeric-argument-switch ,@args)))

(defun switch-to-buffer-dwim (buffer)
  "Display BUFFER in the selected window or, if the buffer is already visible in some window, switch focus to the window containing it."
  (let ((window (get-buffer-window buffer)))
    (if window
        (select-window window)
      (switch-to-buffer buffer))))

(defun switch-or-call (buffer-fn fn &rest args)
  "Use BUFFER-FN to retrieve a buffer and switch to it, or call FN with ARGS."
  (let ((buffer (funcall buffer-fn)))
    (if buffer
        (switch-to-buffer-other-window buffer)
      (apply fn args))))

(defun buffers-with-major-mode (mode)
  "Get a list of buffer with MODE."
  (-filter
   (lambda (buffer) (with-current-buffer buffer (eq mode major-mode)))
   (buffer-list)))

(defun programs-p (&rest xs)
  (-any? 'executable-find xs))

(provide 'init-prelude)

