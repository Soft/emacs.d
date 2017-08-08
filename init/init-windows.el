
(defun other-windows-p ()
  "Are there more than one window"
  (> (length (window-list)) 1))

(use-package winner
  :config (winner-mode))

(defun switch-window-or-buffer ()
  "Switch window or buffer depending on the number of windows"
  (interactive)
  (if (other-windows-p)
      (other-window 1)
    (switch-to-buffer (other-buffer))))

(defun swap-with-largest ()
  "Swap the current window's buffer with the largest window. The largest window is then selected."
  (interactive)
  (let* ((current-win (selected-window))
         (current-buf (window-buffer current-win))
         (largest-win (get-largest-window))
         (largest-buf (window-buffer largest-win)))
    (set-window-buffer largest-win current-buf)
    (set-window-buffer current-win largest-buf)
    (select-window largest-win)))

(defun kill-window-and-maybe-buffer (x)
  "Kill window and optionally its buffer if the universal argument is supplied"
  (interactive "P")
  (quit-window (and x t)))

(provide 'init-windows)
