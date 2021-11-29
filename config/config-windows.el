;;; config-windows.el -*- lexical-binding: t; -*-

(use-package winner
  :defer t
  :init (winner-mode))

(use-package windmove
  :defer t
  :config
  (setq windmove-wrap-around t))

(use-package transpose-frame :defer t)

(defun adq/swap-with-largest ()
  "Swap the current window's buffer with the largest window.
The largest window is then selected."
  (interactive)
  (let* ((current-win (selected-window))
         (current-buf (window-buffer current-win))
         (largest-win (get-largest-window))
         (largest-buf (window-buffer largest-win)))
    (set-window-buffer largest-win current-buf)
    (set-window-buffer current-win largest-buf)
    (select-window largest-win)))

(defun adq/other-windows-p ()
  "Are there more than one window."
  (> (length (window-list)) 1))

(defun adq/switch-window-or-buffer ()
  "Switch window or buffer depending on the number of windows."
  (interactive)
  (if (adq/other-windows-p)
      (other-window 1)
    (switch-to-buffer (other-buffer))))

(bind-key "C-x o" (adq/repeating "o" #'adq/switch-window-or-buffer))
(bind-key "C-<tab>" #'adq/switch-window-or-buffer)

(defhydra adq/hydra-manage-windows nil
  "
^Move^         ^^Window^                 ^Layout^            ^Control
^^^^^^^^^^----------------------------------------------------------------------------------------------
     ^_k_       ^_0_: Delete This         _p_: Previous       _m_: ?m? Menubar    _f_: ?f? Fullscreen
 ^^    |       ^^_1_: Delete Others       _n_: Next           _s_: ?s? Scrollbar
 _h_ - · - _l_   _2_: Split Horizontally  _b_: Balance        _t_: ?t? Tabs
 ^^    |       ^^_3_: Split Vertically    _r_: Clockwise      _T_: ?T? Toolbar
     ^_j_       ^_w_: Swap with largest   _R_: Anticlockwise  _e_: ^^^ Theme
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("r" rotate-frame-clockwise)
  ("R" rotate-frame-anticlockwise)

  ("n" winner-redo)
  ("p" winner-undo)

  ("b" balance-windows :exit t)

  ("f" toggle-frame-fullscreen)
  ("m" toggle-menu-bar-mode-from-frame)
  ("s" toggle-scroll-bar)
  ("t" tab-bar-mode)
  ("T" toggle-tool-bar-mode-from-frame)

  ("e" adq/switch-theme :exit t)

  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("w" adq/swap-with-largest :exit t))

(bind-key "C-c w" #'adq/hydra-manage-windows/body)

(provide 'config-windows)
