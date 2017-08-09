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
  "Makes a function that can be repeated with additional key presses."
  (let ((map (make-sparse-keymap)))
    (cl-labels ((action ()
                        (interactive)
                        (apply fn args)
                        (set-transient-map map)))
      (define-key map (kbd key) #'action)
      #'action)))

(defmacro switch-command (form &rest others)
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

(provide 'init-prelude)
