;;; init-scratch.el --- Scratch -*- lexical-binding: t -*-

;; I didn't really use scratch buffer for emacs lisp so having it be a general
;; purpose notepad is more useful for me.
(setq initial-major-mode 'org-mode
      initial-scratch-message
      (concat
       (replace-regexp-in-string "^" "# " (emacs-version))
       "\n\n"))

;; This could just as well be in init-session.el
(use-package persistent-scratch
  :ensure t
  :init
  (setq persistent-scratch-save-file
        (f-join user-emacs-directory "scratch"))
  (persistent-scratch-setup-default))

;; Patch save-buffer to save scratch using persistent-scratch
;; Possibly a bad idea
(defvar save-buffer-special-alist
  '(("*scratch*" .  (lambda (&optional arg)
                      (interactive "p")
                      (message "Saving scratch...")
                      (persistent-scratch-save))))
  "List of handlers for saving special buffers.")

(defun save-buffer-special (&optional arg)
  (interactive "p")
  (let ((name (buffer-name (current-buffer))))
    (if-let ((handler (assoc name save-buffer-special-alist)))
        (call-interactively (cdr handler) arg)
      (save-buffer arg))))

(global-set-key [remap save-buffer] #'save-buffer-special)

(provide 'init-scratch)
