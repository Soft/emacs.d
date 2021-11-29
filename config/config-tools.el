;;; config-tools.el -*- lexical-binding: t; -*-

(use-package uuidgen :defer t)

(use-package lorem-ipsum :defer t)

(defvar adq/opener "xdg-open"
  "Application used for opening files based on their type.")

(defun adq/open-external ()
  "Open file with an external application."
  (interactive)
  (if-let (file (cond ((eq major-mode 'dired-mode)
                       (ignore-errors (dired-get-file-for-visit)))
                      ((buffer-file-name) (buffer-file-name))
                      (t (read-file-name "File: " nil nil t))))
      (call-process adq/opener nil 0 nil file)
    (error "No file to open")))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-page-resize-factor 1.1))

(provide 'config-tools)
