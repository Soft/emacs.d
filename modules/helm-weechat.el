;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Weehchat integration for Helm

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
  '((name . "WeeChat")
    (candidates . (lambda ()
                    (mapcar #'helm-weechat-format-buffer (weechat-buffer-list))))
    (action . switch-to-buffer-dwim)))

(defun helm-weechat-buffers ()
  (interactive)
  (helm-other-buffer 'helm-source-weechat nil))
