;;; config-term.el -*- lexical-binding: t; -*-

(defvar adq/vterm-background-color
  "#000000"
  "Override background color for `vterm-mode' buffers.")

(defun adq/vterm-setup ()
  "Defaults for vterm buffers."
  ;; This hook hides fringes from windows every time `vterm' buffer is shown.
  (add-hook 'window-buffer-change-functions
            (lambda (window)
              (set-window-fringes window 0 0))
            0
            t)
  (when adq/vterm-background-color
    (face-remap-add-relative
     'default
     :background adq/vterm-background-color)))

(use-package vterm
  :bind
  (("C-c <RET>" . vterm))
  :init
  (add-hook 'vterm-mode-hook #'adq/vterm-setup))

(provide 'config-term)
