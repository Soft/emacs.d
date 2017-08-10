;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Miscellaneous packages

(use-package xkcd
  :commands (xkcd-rand)
  :defer t
  :ensure t
  :preface
  (add-to-list 'recentf-exclude
               "\\.emacs\\.d/xkcd/")
  :config
  (bind-keys
   :map xkcd-mode-map
   ("h" . xkcd-prev)
   ("l" . xkcd-next)
   ("q" . xkcd-kill-buffer)
   ("<escape>" . xkcd-kill-buffer)))

;; FIXME: Telephone-line compatability
(use-package nyan-mode
  :disabled
  :ensure t
  :init
  ;; It's Friday!
  (when (eq (string-to-int (format-time-string "%u")) 5)
    (nyan-mode)
    (nyan-start-animation)
    (nyan-toggle-wavy-trail)))

(use-package dpaste
  :defer t
  :ensure t)

(use-package google-translate
  :defer t
  :ensure t
  :init
  (setq google-translate-default-source-language "fi"
        google-translate-default-target-language "en"
        google-translate-output-destination 'echo-area))

(defun google-translate-with-defaults (d)
  "Query Google Translate with default languages. Reverses the direction if universal argument is supplied."
  (interactive "P")
  (if d
      (google-translate-query-translate-reverse)
    (google-translate-query-translate)))

(bind-key "C-c t" 'google-translate-with-defaults)

(use-package define-word
  :defer t
  :ensure t)

(provide 'init-misc)
