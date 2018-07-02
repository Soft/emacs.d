;;; helm-weechat.el --- WeeChat integration for Helm -*- lexical-binding: t -*-

;; Package-Requires: ((helm) (weechat) (emacs "24.3"))

;;; Commentary:

;; Select WeeChat buffers with Helm.

;;; Code:

(require 'helm)
(require 'weechat)

(defface helm-weechat-channel
  '((t . (:foreground "dark green" :weight bold)))
  "Face used for channel buffers"
  :group 'helm)

(defface helm-weechat-server
  '((t . (:foreground "gray")))
  "Face used for server buffers"
  :group 'helm)

(defun helm-weechat-format-buffer (buffer)
  (let* ((sub (split-string (buffer-name buffer) "\\."))
         (is-server (or (equal (car sub) "server")
                        (equal (car sub) "weechat")))
         (name (car (last sub)))
         (face (if is-server 'helm-weechat-server 'helm-weechat-channel)))
    (cons
     (propertize name 'face face)
     buffer)))

(defvar helm-source-weechat
  (helm-build-sync-source "WeeChat"
    :candidates (lambda ()
                  (mapcar #'helm-weechat-format-buffer
                          (weechat-buffer-list)))
    :action '(("Select buffer" .
               (lambda (buffer)
                 (let ((window (get-buffer-window buffer)))
                   (if window
                       (select-window window)
                     (switch-to-buffer buffer))))))))

;;;###autoload
(defun helm-weechat-buffers ()
  (interactive)
  (helm :sources 'helm-source-weechat
        :buffer "*helm select WeeChat buffer*"))

(provide 'helm-weechat)

;;; helm-weechat.el ends here
