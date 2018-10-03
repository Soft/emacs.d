;;; helm-fd.el --- Use Helm to find files using fd -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (helm))

;;; Commentary:

;; Use Helm to find files using fd

;;; Code:

(require 'helm)

(defvar helm-fd-directory (expand-file-name "~")
  "Current base directory.")

(defvar helm-fd-command "fd"
  "Command that helm-fd will use.")

(defun helm-fd-transformer (candidates _source)
  "Candidate transformer for `helm-fd'."
  (cl-loop for candidate in candidates
           when (stringp candidate)
           collect
           (let ((dir
                  (string-remove-prefix helm-fd-directory
                                        (file-name-directory candidate)))
                 (filename (file-name-nondirectory candidate)))
             (cons (if dir (concat (propertize dir 'face 'font-lock-comment-face)
                                   (propertize filename 'face 'font-lock-type-face))
                     (propertize filename 'face 'font-lock-type-face))
                   candidate))))

(defvar helm-fd-source
  (helm-build-async-source "fd"
    :candidates-process
    (lambda ()
      (start-process "fd" nil helm-fd-command
                     "--follow" "--color" "never" "--"
                     helm-pattern
                     helm-fd-directory))
    :header-name
    (lambda (name)
      (format "%s in %s" name helm-fd-directory))
    :requires-pattern 3
    :action '(("Find file" . find-file))
    :filtered-candidate-transformer #'helm-fd-transformer)
  "Source for searching files with fd.")

;;;###autoload
(defun helm-fd (d)
  "Find files with fd."
  (interactive "P")
  (setq helm-fd-directory
        (if d (read-directory-name default-directory)
          (expand-file-name "~")))
  (helm :sources #'helm-fd-source
        :prompt "fd: "
        :buffer "*fd*"))


(provide 'helm-fd)

;;; helm-fds.el ends here
