;;; init-local.el --- Load emacs lisp from adequate emacs.d's "local" -*- lexical-binding: t -*-

;;; Commentary:

;; Adequate supports loading user's personal customizations from its `local'
;; directory. The load order is based on the file names.

;; The files are loaded after init completes.

;;; Code:

(defvar adq/load-local-ignored
  '("customize.el")
  "Files that will not be loaded by adq/load-local-files.")

(defvar adq/loaded-local-files
  '())

;; TODO: Implement natural sorting
(defun adq/load-local-files (&optional force)
  "Load files from adequate emacs.d's \"local\" directory. If
FORCE is non-nil, load files even if they have already been
loaded. Returns a list of files that were loaded."
  (-map 
   (lambda (path)
     (when adq/emacs-debug
       (message "Loading %s" path))
     (add-to-list 'adq/loaded-local-files path)
     (load path nil nil t)
     path)
   (-sort (lambda (a b) (string< (f-filename a)
                            (f-filename b)))
          (f--files (f-join adq/init-directory "local")
                    (and
                     (equal (f-ext it) "el")
                     (not (member (f-filename it) adq/load-local-ignored))
                     (or force
                         (not (member it adq/loaded-local-files))))))))

(add-hook 'after-init-hook #'adq/load-local-files)

(provide 'init-local)

;;; init-local.el ends here
