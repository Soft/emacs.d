;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Miscellaneous packages

(use-package xkcd
  :commands (xkcd-rand)
  :defer t
  :ensure t
  :preface
  (add-to-list 'recentf-exclude
               "\\.emacs\\.d/xkcd/")
  :bind (("C-c x X" . xkcd-rand))
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

;; Remember to set sunshine-appid & sunshine-location
(use-package sunshine
  :defer t
  :ensure t
  :bind (("C-c x w" . sunshine-forecast))
  :config
  (setq sunshine-units 'metric
        sunshine-show-icons t))

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

(bind-key "C-c x t" 'google-translate-with-defaults)

(use-package define-word
  :defer t
  :ensure t
  :bind (("C-c x d" . define-word-at-point)))

(use-package nov
  :ensure nov
  :defer t
  :mode (("\\.epub\\'" . nov-mode))
  :config
  (bind-keys
   :map nov-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . nov-scroll-down)
   ("K" . nov-scroll-up)))

(provide 'init-misc)
