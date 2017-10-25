;;; init-scratch.el --- Scratch -*- lexical-binding: t -*-

;;; Commentary:

;; Make scratch buffer a general purpose persistent notepad.
;; This tries to patch Emacs to keep persistent scratch.

;;; Code:

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
(add-to-list
 'adq/save-buffer-special-alist
 '("*scratch*" .  (lambda (&optional arg)
                    (interactive "p")
                    (message "Saving scratch...")
                    (persistent-scratch-save))))

;; For some reason the standard org-mode hook will not fire
(add-hook
 'after-init-hook
 (lambda ()
   (with-current-buffer "*scratch*"
     (run-hooks 'org-mode-hook))))

(provide 'init-scratch)

;;; init-scratch.el ends here
