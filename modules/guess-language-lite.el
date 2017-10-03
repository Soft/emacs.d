;;; guess-language-lite.el --- Run commands based on buffers language -*- lexical-binding: t -*-

;; Package-Requires: ((guess-language) (emacs "24.3"))

;;; Commentary:

;; This minor mode builds upon the guess-language-mode by using it for the
;; actual language detection. What differentiates this mode is that it doesn't
;; not use flyspell for triggering the detection. Additionally, it does not try
;; to support multiple languages per buffer.

;; To put it simply, this mode tries to periodically guess the language of the
;; buffer and runs a hook when it does so.

;; This is still very much experimental

;;; Code:

(require 'guess-language)

(defvar-local gll-buffer-timer nil)

(defvar gll-language-identified-functions '()
  "Hook to be called when buffer's language is identified.")

(defvar gll-buffer-minimum-size 100
  "Minimum number of characters")

(defvar gll-check-interval 30
  "Identification interval.")

(defun gll-identify (buffer)
  (when (buffer-live-p buffer) ; This is kind of bad, we should cancel the timer if buffer has been killed
    (with-current-buffer buffer
      (when (<= gll-buffer-minimum-size (buffer-size))
        (let* ((lang (guess-language-region (point-min) (point-max))))
          (message "Identified %s as %s"
                   (or (buffer-file-name)
                       (buffer-name))
                   lang)
          (setq-local gll-buffer-language lang)
          (run-hook-with-args 'gll-language-identified-functions lang)
          (when gll-buffer-timer
            (cancel-timer gll-buffer-timer)))
        t))))

;;;###autoload
(define-minor-mode gll-guess-language-lite-mode
  "Execute actions based on buffer's language."
  :lighter " gll"
  (if gll-guess-language-lite-mode
      (let ((buffer (current-buffer)))
        (unless (gll-identify buffer)
          (setq-local gll-buffer-timer
                      (run-at-time gll-check-interval
                                   gll-check-interval
                                   #'gll-identify buffer))
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (when gll-buffer-timer
                        (cancel-timer gll-buffer-timer)))
                    t t)))
    (when gll-buffer-timer
      (cancel-timer gll-buffer-timer))))

(provide 'guess-language-lite)

(provide 'guess-language-lite)

;;; guess-language-lite.el ends here
