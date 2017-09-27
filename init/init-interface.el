;;; init-interface.el --- Interface related settings -*- lexical-binding: t -*-

(setq
 inhibit-splash-screen t
 inhibit-startup-message t)

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
  ;; Make display-time-mode play nice with other modes that modify
  ;; global-mode-string
  (add-hook
   'display-time-hook
   (lambda()
     (setq display-time-string (concat display-time-string " "))))
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

(use-package centered-window-mode
  :ensure t
  :bind (("C-c w c" . centered-window-mode)))

(use-package highlight-indent-guides
  :ensure t
  :bind (("C-c x g" . highlight-indent-guides-mode)))

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

(use-package fill-column-indicator
  :ensure t
  :defer t)

(provide 'init-interface)

