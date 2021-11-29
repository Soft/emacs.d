;;; config-buffers.el -*- lexical-binding: t -*-

(defvar adq/immortal-buffers
  '("*scratch*" "*Messages*" "*dashboard*")
  "Buffers that cannot be killed.")

(defun adq/kill-current-buffer-keep-immortal ()
  "Protect immortal buffers from being killed."
  (interactive)
  (when-let ((name (buffer-name)))
    (if (member name adq/immortal-buffers)
        (bury-buffer)
      (kill-current-buffer))))

(global-set-key [remap kill-current-buffer]
                #'adq/kill-current-buffer-keep-immortal)

(provide 'config-buffers)
