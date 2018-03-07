;;; helm-org-files.el --- Use Helm to find Org files -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2") (f) (helm))

;;; Commentary:

;; Use Helm to find and visit Org files from `org-directory'.

;;; Code:

(require 'rx)
(require 'f)
(require 'org)
(require 'helm)

(defmacro helm-org-files--make-keyword-regex (keyword)
  `(rx (and line-start "#+" ,(regexp-quote keyword) ":" (0+ space) (submatch (1+ not-newline)))))

(defconst helm-org-files--title-regex
  (helm-org-files--make-keyword-regex "TITLE"))

(defconst helm-org-files--author-regex
  (helm-org-files--make-keyword-regex "AUTHOR"))

(defun helm-org-files--title-for-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (if (re-search-forward helm-org-files--title-regex nil t)
        (let ((title (propertize  (match-string 1) 'face 'org-document-title)))
          (goto-char (point-min))
          (if (re-search-forward helm-org-files--author-regex nil t)
              (let ((author (propertize (match-string 1) 'face 'org-document-info)))
                (format "%s: %s" author title))
            title))
      (file-name-nondirectory path))))

(defun helm-org-files--files (path)
  (sort (f--files path (equal (f-ext it) "org") t)
        #'file-newer-than-file-p))

(defun helm-org-files--candidates ()
  (mapcar (lambda (path)
            (cons (helm-org-files--title-for-file path) path))
          (helm-org-files--files org-directory)))

;;;###autoload
(defun helm-org-files ()
  "Use Helm to find and visit Org files from `org-directory'."
  (interactive)
  (helm :sources (helm-build-sync-source "org-files"
                   :candidates #'helm-org-files--candidates
                   :action (helm-make-actions
                            "Find file" #'find-file))
        :buffer "*helm org-files*"))

(provide 'helm-org-files)

;;; helm-org-files.el ends here
