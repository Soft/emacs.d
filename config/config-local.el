;;; config-local.el -*- lexical-binding: t; -*-

(defvar adq/loaded-local-files
  (make-hash-table :test 'equal))

(defvar adq/early-local-files-directory
  (concat adq/adequate-directory "local/early")
  "Directory for local files that will be loaded early in the initialization process.")

(defvar adq/late-local-files-directory
  (concat adq/adequate-directory "local/late")
  "Directory for local files that will be loaded late in the initialization process.")

(defun adq/load-local-files (path &optional force)
  (thread-last (directory-files path t "\\.el\\'")
    (seq-filter (lambda (path)
                  (or force
                      (not (gethash path adq/loaded-local-files)))))
    (seq-sort-by #'file-name-base #'string<)
    (mapcar
     (lambda (path)
       (when adq/emacs-debug
         (message "Loading %s" path))
       (puthash path t adq/loaded-local-files)
       (load path nil nil t)
       path))))

(adq/load-local-files adq/early-local-files-directory)

(add-hook 'after-init-hook
          (apply-partially #'adq/load-local-files adq/late-local-files-directory))

(provide 'config-local)
