;;; helm-fd.el --- Use Helm to find files using fd -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (helm))

;;; Commentary:

;; Use Helm to find files using fd

;;; Code:

(require 'helm)
(require 'ansi-color)

(defvar helm-fd-directory (expand-file-name "~")
  "Current base directory.")

(defvar helm-fd-command "fd"
  "Command that helm-fd will use.")

(defun helm-fd-transformer (candidates _source)
  "Candidate transformer for `helm-fd'."
  (cl-loop for candidate in candidates
           when (stringp candidate)
           collect
           (let ((colored (let ((ansi-color-context nil))
                            (ansi-color-apply candidate))))
             (cons colored (substring-no-properties colored)))))

(defvar helm-fd-source
  (helm-build-async-source "fd"
    :candidates-process
    (lambda ()
      (start-process "fd" nil helm-fd-command
                     "--follow" "--color" "always" "--"
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
        :prompt (format "fd %s: " helm-fd-directory)
        :buffer "*fd*"))


(provide 'helm-fd)

;;; helm-fds.el ends here
