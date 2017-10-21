;;; google-translate-repl.el --- REPL for Google translate -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2") (google-translate))

;;; Commentary:

;; Use Google Translate with REPL like interface. The implementation is heavily
;; inspired by IELM.

;; As a side note, it would be nice if there was a library for doing comint like
;; interfaces without assumption of external programs.

;;; Code:

(require 's)
(require 'google-translate)

(defvar google-translate-repl-source-language "en"
  "Google translate REPL source language.")

(defvar google-translate-repl-target-language "fi"
  "Google translate REPL target language.")

;; Do not set this directly, modify `google-translate-repl-prompt-function' instead.
(defvar google-translate-repl--prompt)

(defvar google-translate-repl--input)

(defun google-translate-repl-supported-language-p (lang)
  (and (rassoc lang google-translate-supported-languages-alist) t))

(defun google-translate-repl-code-to-name (lang)
  (car (rassoc lang google-translate-supported-languages-alist)))

(defun google-translate-repl-format-prompt ()
  "Reset Google translate REPL prompt based on current source and target languages."
  (format "%s→%s> "
          google-translate-repl-source-language
          google-translate-repl-target-language))

(defvar google-translate-repl-prompt-function #'google-translate-repl-format-prompt
  "Function to use for creating `google-translate-repl--prompt'.")

(defun google-translate-repl-strip-command (input)
  "Strip command name from its arguments."
  (second (s-split-up-to " " input 1 t)))

(defun google-translate-repl-parse-input (input)
  "Separate commands from input."
  (pcase input
    ((pred (string-prefix-p ":source "))
     (cons 'source (google-translate-repl-strip-command input)))
    ((pred (string-prefix-p ":target "))
     (cons 'target (google-translate-repl-strip-command input)))
    (":swap" 'swap)
    (_ input)))

(defun google-translate-repl-process-input (input)
  "Process input from REPL."
  (let (output)
    (pcase (google-translate-repl-parse-input input)
      (`(source . ,lang)
       (if (and lang (google-translate-repl-supported-language-p lang))
           (setq google-translate-repl-source-language lang
                 output (format "Source language set to %s"
                                (google-translate-repl-code-to-name lang)))
         (setq output "Invalid source language."))
       (google-translate-repl-reset-prompt))
      (`(target . ,lang)
       (if (and lang (google-translate-repl-supported-language-p lang))
           (setq google-translate-repl-target-language lang
                 output (format "Target language set to %s"
                                (google-translate-repl-code-to-name lang)))
         (setq output "Invalid target language."))
       (google-translate-repl-reset-prompt))
      ('swap
       (let ((old-target google-translate-repl-target-language))
         (setq google-translate-repl-target-language google-translate-repl-source-language
               google-translate-repl-source-language old-target)
         (google-translate-repl-reset-prompt)
         (setq output
               (format "Source language set to %s; Target language set to %s"
                       (google-translate-repl-code-to-name
                        google-translate-repl-source-language)
                       (google-translate-repl-code-to-name
                        google-translate-repl-target-language)))))
      ((pred (stringp)) 
       (let ((cleaned (s-trim input)))
         (if (s-present? cleaned)
             (setq output
                   (with-temp-buffer
                     (google-translate-translate
                      google-translate-repl-source-language
                      google-translate-repl-target-language
                      cleaned
                      'current-buffer)
                     (buffer-string)))
           (setq output "")))))
    (comint-output-filter (get-buffer-process (current-buffer))
                          (concat  output "\n" google-translate-repl--prompt))))

(defvar google-translate-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'google-translate-repl-return)
    map)
  "Keymap for Google Translate REPL.")

(defvaralias 'google-translate-repl-mode-map 'google-translate-repl-map)

(defun google-translate-repl-return ()
  "Handle enter."
  (interactive)
  (comint-send-input)
  (google-translate-repl-process-input google-translate-repl--input))

(defun google-translate-repl-reset-prompt ()
  "Reset internal prompt variables."
  (let ((prompt (funcall google-translate-repl-prompt-function)))
    (setq comint-prompt-regexp (concat "^" (regexp-quote prompt))
          google-translate-repl--prompt prompt)))

(define-derived-mode google-translate-repl-mode comint-mode "translate"
  "Google translate"
  (google-translate-repl-reset-prompt)
  (setq comint-input-sender (lambda (proc input)
                              (setq google-translate-repl--input input))
        comint-process-echoes nil)
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "translate" (current-buffer) "hexl")
      (file-error (start-process "translate" (current-buffer) "cat")))
    (let ((process (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag process nil)
      (goto-char (point-max))
      (comint-output-filter process google-translate-repl--prompt)
      (set-process-filter process 'comint-output-filter))))

;;;###autoload
(defun google-translate-repl ()
  "Open or switch to Google Translate REPL."
  (interactive)
  (unless (comint-check-proc "*translate*")
    (with-current-buffer (get-buffer-create "*translate*")
      (google-translate-repl-mode)))
  (pop-to-buffer-same-window "*translate*"))

(provide 'google-translate-repl)

;;; google-translate-repl.el ends here
