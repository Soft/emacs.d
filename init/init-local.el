;;; init-local.el --- Load emacs lisp from adequate emacs.d's "local" -*- lexical-binding: t -*-

(defvar load-local-ignored
  '("customize.el")
  "Files that will not be loaded by load-local-files.")

(defvar loaded-local-files
  '())

;; TODO: Implement natural sorting
(defun load-local-files (&optional force)
  "Load files from adequate emacs.d's \"local\" directory. If FORCE is non-nil, load files even if they have already been loaded. Returns a list of files that were loaded."
  (-map 
   (lambda (path)
     (when emacs-debug
       (message "Loading %s" path))
     (add-to-list 'loaded-local-files path)
     (load path nil nil t)
     path)
   (-sort (lambda (a b) (string< (f-filename a)
                            (f-filename b)))
          (f--files (f-join init-directory "local")
                    (and
                     (equal (f-ext it) "el")
                     (not (member (f-filename it) load-local-ignored))
                     (or force
                         (not (member it loaded-local-files))))))))


(run-with-idle-timer 1 nil #'load-local-files)

(provide 'init-local)
