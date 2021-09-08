;;; helm-zoxide.el --- Use Helm to navigate directories using zoxide -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (helm))

;;; Commentary:

;; Use Helm to navigate directories using zoxide.

;;; Code:

(require 'helm)

(defvar helm-zoxide-command "zoxide"
  "Command that helm-zoxide will use.")

(defvar helm-zoxide-source
  (helm-build-async-source "zoxide"
    :candidates-process
    (lambda ()
      (start-process "zoxide" nil helm-zoxide-command
                     "query" "--list" "--"
                     helm-pattern))
    :action '(("Change directory" . cd)))
  "Source for changing directories with zoxide.")

;;;###autoload
(defun helm-zoxide (d)
  "Change directory with zoxide."
  (interactive "P")
  (helm :sources #'helm-zoxide-source
        :prompt "Change directory to: "
        :buffer "*zoxide*"))

(defun helm-zoxide-find-file-hook-zoxide-add-directory ()
  "find-file-hook that can be used to add the directories of
visited files to zoxide database."
  (when-let* ((filename (buffer-file-name))
              (directory (file-name-directory filename)))
    (call-process helm-zoxide-command nil nil nil "add" directory)))

;;;###autoload
(defun helm-zoxide-enable-find-file-hook ()
  "Enable recording the directories of visited files to zoxide
database."
  (interactive)
  (add-hook 'find-file-hook
            #'helm-zoxide-find-file-hook-zoxide-add-directory))


(provide 'helm-zoxide)

;;; helm-zoxide.el ends here
