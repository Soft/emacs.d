;;; config-local.el -*- lexical-binding: t; -*-

(defvar adq/loaded-local-files
  (make-hash-table :test 'equal))

(defvar adq/local-files-directory
  (concat adq/adequate-directory "local")
  "Directory from where `adq/load-local-files' will load additional configuration.")

(defun adq/load-local-files (&optional force)
  (thread-last (directory-files adq/local-files-directory t "\\.el\\'")
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

(add-hook 'after-init-hook #'adq/load-local-files)

(provide 'config-local)
