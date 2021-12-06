;;; prettify-quotes.el --- Replace plain quotes with pretty quotes -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Replace plain quotes with pretty quotes.

;;; Code:

(defun prettify-quotes-string (string)
  "Prettify quotes in a string"
  (let ((next 'open))
    (concat
     (mapcar
      (lambda (c)
        (if (equal c ?\")
            (pcase next
              ('open
               (progn
                 (setf next 'close)
                 ?“))
              ('close
               (progn
                 (setf next 'open)
                 ?”)))
          c))
      string))))

;;;###autoload
(defun prettify-quotes-region (start end)
  "Prettify quotes in region."
  (interactive "r")
  (save-excursion
    (let* ((region (buffer-substring-no-properties start end))
           (prettified (prettify-quotes-string region)))
      (kill-region start end)
      (insert prettified))))

(provide 'prettify-quotes)

;;; prettify-quotes.el ends here
