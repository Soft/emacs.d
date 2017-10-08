;;; init-buffers.el --- Buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Maybe it would after all be better to advice the functions instead of just
;; remaping the bindings since it would make it harder to ignore these.

;;; Code:

(defvar save-buffer-special-alist '()
  "List of handlers for saving special buffers.")

(defun save-buffer-special (&optional arg)
  (interactive "p")
  (let ((name (buffer-name (current-buffer))))
    (if-let ((handler (assoc name save-buffer-special-alist)))
        (call-interactively (cdr handler) arg)
      (save-buffer arg))))

(global-set-key [remap save-buffer] #'save-buffer-special)

;; Protect certain buffers from being killed
(defvar immortal-buffers
  '("*scratch*" "*Messages*" "*dashboard*")
  "Buffers that cannot be killed.")

(defun kill-buffer-keep-immortal (buffer)
  "Protect immortal buffers from being killed."
  (interactive (list (current-buffer)))
  (let ((name (buffer-name buffer)))
    (if (member name immortal-buffers)
        (progn
          (message "%s is immortal and cannot be killed." name)
          (call-interactively #'bury-buffer buffer))
      (kill-buffer buffer))))

(global-set-key [remap kill-buffer] #'kill-buffer-keep-immortal)

(provide 'init-buffers)

;;; init-buffers.el ends here
