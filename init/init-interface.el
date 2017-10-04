;;; init-interface.el --- Interface related settings -*- lexical-binding: t -*-

(setq
 inhibit-splash-screen t
 inhibit-startup-message t)

(setq-default
 inhibit-x-resources t
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

(if-supported tool-bar-mode -1)
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
  (if (eq window-system 'x)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "toggle-fulscreen is only available on X windows systems.")))

(bind-key "<f11>" #'toggle-fullscreen)

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

(defvar beacon-recalibration-percent 10
  "How many percents should beacon color differ from background color.")

(use-package beacon
  :ensure t
  :init
  (beacon-mode)
  :config
  (require 'color)
  (defun beacon-recalibrate-color ()
    (if-let ((background (face-attribute 'default :background)))
        (setq beacon-color
              (if (> (color-distance background "#ffffff")
                     (color-distance background "#000000"))
                  (color-lighten-name background beacon-recalibration-percent)
                (color-darken-name background beacon-recalibration-percent)))))
  (advice-add #'load-theme :after (lambda (&rest args) (beacon-recalibrate-color))))

(use-package fill-column-indicator
  :ensure t
  :defer t)

(provide 'init-interface)

