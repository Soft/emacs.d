
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message
      (concat
       (replace-regexp-in-string "^" ";; " (emacs-version))
       "\n\n"))

(setq use-dialog-box nil
      echo-keystrokes 0.1
      xterm-mouse-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(setq frame-title-format '((:eval (concat
                                   (if (buffer-file-name)
                                       (abbreviate-file-name (buffer-file-name))
                                     "%b")
                                   " - emacs"))))

(fringe-mode '(4 . 0))

(setq display-time-24hr-format t)
(display-time-mode)

(defun set-frame-alpha (number)
  (interactive "nAlpha: ")
  (if (<= 10 number 100)
      (set-frame-parameter nil 'alpha number)
    (error "Invalid alpha")))

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(bind-key "<f11>" 'toggle-fullscreen)

(provide 'init-interface)

