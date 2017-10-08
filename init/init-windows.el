;;; init-windows.el --- Window management -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for window management.

;;; Code:

(defun other-windows-p ()
  "Are there more than one window."
  (> (length (window-list)) 1))

(use-package winner
  :init (winner-mode))

(use-package windmove
  :config
  (setq windmove-wrap-around t))

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

;; FIXME: Nicer modeline
(use-package zoom-window
  :defer t
  :ensure t
  :bind (("C-x 4 z" . zoom-window-zoom)))

(use-package transpose-frame
  :defer t
  :ensure t)

(defhydra hydra-manage-windows nil
  "
^Move^         ^^Window^                 ^Layout^            ^Control
^^^^^^^^^^----------------------------------------------------------------------------------------------
     ^_k_       ^_0_: Delete This         _p_: Previous       _m_: Menubar    _z_: Zoom
 ^^    ↑       ^^_1_: Delete Others       _n_: Next           _s_: Scrollbar  _f_: Fullscreen
 _h_ ← · → _l_   _2_: Split Horizontally  _b_: Balance        _t_: Toolbar    _c_: Center
 ^^    ↓       ^^_3_: Split Vertically    _r_: Clockwise      
     ^_j_                              ^^^_R_: Anticlockwise  
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("r" rotate-frame-clockwise)
  ("R" rotate-frame-anticlockwise)

  ("n" winner-redo)
  ("p" winner-undo)
  ("b" balance-windows)

  ("z" zoom-window-zoom)
  ("f" toggle-fullscreen)
  ("m" toggle-menu-bar-mode-from-frame)
  ("s" toggle-scroll-bar)
  ("t" toggle-tool-bar-mode-from-frame)
  ("c" centered-window-mode)

  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right))

(bind-key "C-c w" #'hydra-manage-windows/body)

(provide 'init-windows)

;;; init-windows.el ends here
