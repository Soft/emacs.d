
(defmacro dlet* (varlist &rest body)
  "Destructuring let*"
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

(defun repeating (key fn &rest args)
  "Makes a function that can be repeated with additional key presses"
  (let ((map (make-sparse-keymap)))
    (cl-labels ((action ()
                        (interactive)
                        (apply fn args)
                        (set-transient-map map)))
      (define-key map (kbd key) #'action)
      #'action)))

(provide 'init-prelude)
