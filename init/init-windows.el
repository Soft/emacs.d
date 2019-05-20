;;; init-windows.el --- Window management -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for window management.

;;; Code:

(defun adq/other-windows-p ()
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
(defun adq/switch-window-or-buffer ()
  "Switch window or buffer depending on the number of windows."
  (interactive)
  (if (adq/other-windows-p)
      (other-window 1)
    (switch-to-buffer (other-buffer))))

(use-package golden-ratio
  :ensure t
  :init (golden-ratio-mode))

(defun adq/swap-with-largest ()
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
 ("C-x 4 s" . adq/swap-with-largest)
 ("C-<tab>" . adq/switch-window-or-buffer))

(defun adq/next-frame ()
  "Switch to the next frame."
  (interactive)
  (other-frame 1))

(defun adq/switch-frame-or-window-or-buffer (d)
  "Switches frames if universal argument is supplied. If the
argument is not supplied switches windows if the frame has more
than one window. If the frame only has a single window, switches
buffers."
  (interactive "P")
  (funcall-interactively
   (if d
       (adq/repeating "o" #'adq/next-frame)
     (adq/repeating "o" #'adq/switch-window-or-buffer))))

(global-set-key (kbd "C-x 4 R")
                (adq/repeating "R" #'rotate-frame-clockwise))
;; Maybe this is a little too much functionality for a single key
(global-set-key (kbd "C-x o") #'adq/switch-frame-or-window-or-buffer)
(global-set-key (kbd "C-x 5 o")
                (adq/repeating "o" #'adq/next-frame))

(defun adq/kill-window-and-maybe-buffer (x)
  "Kill window and optionally its buffer if the universal argument is supplied"
  (interactive "P")
  (quit-window (and x t)))

(defvar adq/zoom-window-recalibration-percent 10
  "How many percents should mode line color differ when zoomed
  from the regular color.")

(use-package zoom-window
  :defer t
  :ensure t
  :functions (zoom-window--enable-p)
  :bind (("C-x 4 z" . zoom-window-zoom))
  :config
  (defun adq/zoom-window-recalibrate-color ()
    (setq zoom-window-mode-line-color
          (adq/color-derive adq/zoom-window-recalibration-percent
                            (face-background 'mode-line))))
  (adq/zoom-window-recalibrate-color)
  (add-hook 'adq/switch-theme-hook #'adq/zoom-window-recalibrate-color))

(use-package transpose-frame
  :defer t
  :ensure t)

(use-package all-monitors
  :defer t
  :functions (all-monitors-frames-on-all-monitors)
  :commands (all-monitors-fill-all-monitors))

(defhydra adq/hydra-manage-windows nil
  "
^Move^         ^^Window^                 ^Layout^            ^Control
^^^^^^^^^^----------------------------------------------------------------------------------------------
     ^_k_       ^_0_: Delete This         _p_: Previous       _m_: ?m? Menubar    _z_: ?z? Zoom
 ^^    ↑       ^^_1_: Delete Others       _n_: Next           _s_: ?s? Scrollbar  _f_: ?f? Fullscreen
 _h_ ← · → _l_   _2_: Split Horizontally  _b_: Balance        _t_: ?t? Toolbar    _c_: ?c? Center
 ^^    ↓       ^^_3_: Split Vertically    _r_: Clockwise      _e_:     Theme      _g_: ?g? Golden ratio
     ^_j_       ^_w_: Swap with largest   _R_: Anticlockwise  
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

  ("z" zoom-window-zoom
   (if (and (featurep 'zoom-window)
            (zoom-window--enable-p))
       "[x]" "[ ]"))
  ("f" adq/toggle-fullscreen
   (if (frame-parameter nil 'fullscreen)
       "[x]" "[ ]"))
  ("m" toggle-menu-bar-mode-from-frame
   (if (bound-and-true-p menu-bar-mode)
       "[x]" "[ ]"))
  ("s" toggle-scroll-bar
   (if (frame-parameter nil 'vertical-scroll-bars)
       "[x]" "[ ]"))
  ("t" toggle-tool-bar-mode-from-frame
   (if (bound-and-true-p tool-bar-mode)
       "[x]" "[ ]"))
  ("c" centered-window-mode
   (if (bound-and-true-p centered-window-mode)
       "[x]" "[ ]"))
  ("g" golden-ratio-mode
   (if (bound-and-true-p golden-ratio-mode)
       "[x]" "[ ]"))
  ("e" adq/switch-theme :exit t)

  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("w" adq/swap-with-largest :exit t))

(bind-key "C-c w" #'adq/hydra-manage-windows/body)

(provide 'init-windows)

;;; init-windows.el ends here
