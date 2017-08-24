;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Interface related settings

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message
 (concat
  (replace-regexp-in-string "^" ";; " (emacs-version))
  "\n\n"))

(setq-default
 use-dialog-box nil
 use-file-dialog nil
 echo-keystrokes 0.1
 xterm-mouse-mode 1
 cursor-in-non-selected-windows nil
 mouse-yank-at-point t
 ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 select-enable-clipboard t
 select-enable-primary t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(setq frame-title-format
      '((:eval (concat
                (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b")
                " - emacs@"
                system-name))))

(fringe-mode '(8 . 0))

(use-package all-the-icons
  :ensure t)

(use-package time
  :init
  (setq display-time-24hr-format t
        display-time-default-load-average nil)
  (display-time-mode))

(defun set-frame-alpha (number)
  (interactive "nAlpha: ")
  (if (<= 10 number 100)
      (set-frame-parameter nil 'alpha number)
    (error "Invalid alpha")))

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(bind-keys
 ("<f11>" . toggle-fullscreen)
 ("C-c w f" . toggle-fullscreen)
 ("C-c w m" . toggle-menu-bar-mode-from-frame)
 ("C-c w s" . toggle-scroll-bar)
 ("C-c w t" . toggle-tool-bar-mode-from-frame))

(use-package writeroom-mode
  :defer t
  :ensure t
  :config
  (setq writeroom-width 120))

(use-package centered-window-mode
  :defer t
  :ensure t)

(use-package highlight-indent-guides
  :defer t
  :ensure t)

;; Disabled for now
(use-package volatile-highlights
  :disabled
  :ensure t
  :init
  (volatile-highlights-mode)
  :config
  (vhl/define-extension 'evil
                        'evil-paste-after
                        'evil-paste-before
                        'evil-paste-pop
                        'evil-move)
  (vhl/install-extension 'evil))

(provide 'init-interface)

