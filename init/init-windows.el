;;; init-windows.el --- Window management -*- lexical-binding: t -*-

(defun other-windows-p ()
  "Are there more than one window."
  (> (length (window-list)) 1))

(use-package winner
  :config (winner-mode))

;; TODO:
;; Skip buffers with certain modes without C-u (eg. neotree)
;; Also it would be nice if we switched between frames as well.
(defun switch-window-or-buffer ()
  "Switch window or buffer depending on the number of windows."
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

(bind-keys
 ("C-x 4 s" . swap-with-largest)
 ("C-<tab>" . switch-window-or-buffer))

(global-set-key (kbd "C-x 4 R")
                (repeating "R" #'rotate-frame-clockwise))
(global-set-key (kbd "C-x o")
                (repeating "o" #'switch-window-or-buffer))

(defun kill-window-and-maybe-buffer (x)
  "Kill window and optionally its buffer if the universal argument is supplied"
  (interactive "P")
  (quit-window (and x t)))

(use-package zoom-window
  :defer t
  :ensure t
  :bind (("C-x 4 z" . zoom-window-zoom)))

(use-package transpose-frame
  :defer t
  :ensure t)

(provide 'init-windows)
