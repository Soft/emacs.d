;;; helm-org-files.el --- Use Helm to find Org files -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2") (f) (helm))

;;; Commentary:

;; Use Helm to find and visit Org files from `org-directory'.

;;; Code:

(require 'rx)
(require 'f)
(require 'helm)

(defconst helm-org-files--title-regex
  (rx (and line-start "#+TITLE:" (0+ space) (submatch (1+ not-newline)))))

(defun helm-org-files--title-for-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (if (re-search-forward helm-org-files--title-regex nil t)
        (match-string 1)
      (file-name-nondirectory path))))

(defun helm-org-files--files (path)
  (f--files path (equal (f-ext it) "org") t))

(defun helm-org-files--candidates ()
  (mapcar (lambda (path)
            (cons (helm-org-files--title-for-file path) path))
          (helm-org-files--files org-directory)))

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
