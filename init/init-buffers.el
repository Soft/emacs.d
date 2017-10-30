;;; init-buffers.el --- Buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Maybe it would after all be better to advice the functions instead of just
;; remaping the bindings since it would make it harder to ignore these.

;;; Code:

(defvar adq/save-buffer-special-alist '()
  "List of handlers for saving special buffers.")

(defun adq/save-buffer-special (&optional arg)
  (interactive "p")
  (let ((name (buffer-name (current-buffer))))
    (if-let ((handler (assoc name adq/save-buffer-special-alist)))
        (call-interactively (cdr handler) arg)
      (save-buffer arg))))

(global-set-key [remap save-buffer] #'adq/save-buffer-special)

;; Protect certain buffers from being killed
(defvar adq/immortal-buffers
  '("*scratch*" "*Messages*" "*dashboard*")
  "Buffers that cannot be killed.")

(defun adq/kill-buffer-keep-immortal (buffer)
  "Protect immortal buffers from being killed."
  (interactive (list (current-buffer)))
  (let ((name (buffer-name buffer)))
    (if (member name adq/immortal-buffers)
        (progn
          (message "%s is immortal and cannot be killed." name)
          (call-interactively #'bury-buffer buffer))
      (kill-buffer buffer))))

(global-set-key [remap kill-buffer] #'adq/kill-buffer-keep-immortal)

(defvar adq/prevent-editing-elpa t
  "Prevent editing files from Elpa.")

;; Make files belonging to Elpa packages read only
(add-hook 'find-file-hook
          (lambda ()
            (when (and adq/prevent-editing-elpa
                       (f-descendant-of-p (buffer-file-name) package-user-dir))
              (setq-local buffer-read-only t))))

(provide 'init-buffers)

;;; init-buffers.el ends here
