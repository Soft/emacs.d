;;; urldecode.el --- URL decode region -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; URL Decode region

;;; Code:

(require 'url)

;;;###autoload
(defun urldecode-replace-region (from to)
  "URL decode region."
  (interactive "r")
  (save-excursion
    (insert
     (url-unhex-string
      (delete-and-extract-region from to)
      t))))

(provide 'urldecode)

;;; urldecode.el ends here
