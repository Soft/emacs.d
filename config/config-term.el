;;; config-term.el -*- lexical-binding: t; -*-

(defvar adq/vterm-face-overrides
  `(
    :background "#000000"
    :foreground "#ffffff"
    :family ,adq/term-font-family
    )
  "Face overrides for `vterm-mode' buffers.")

(defun adq/vterm-setup ()
  "Defaults for vterm buffers."
  ;; This hook hides fringes from windows every time `vterm' buffer is shown.
  (add-hook 'window-buffer-change-functions
            (lambda (window)
              (set-window-fringes window 0 0))
            0 t)
  (when adq/vterm-face-overrides
    (apply #'face-remap-add-relative
           (append (list 'default)
                   adq/vterm-face-overrides))))

(defun adq/toggle-vterm ()
  "Launch or focus vterm."
  (interactive)
  (if-let ((buffer (car (adq/buffers-with-major-mode 'vterm-mode))))
      (if-let ((window (get-buffer-window buffer)))
          (if (eq window (selected-window))
              (bury-buffer)
            (select-window window))
        (switch-to-buffer buffer))
    (vterm)))

(use-package vterm
  :defer t
  :config
  (add-hook 'vterm-mode-hook #'adq/vterm-setup))

(bind-key "C-c <RET>" #'adq/toggle-vterm)

(provide 'config-term)
