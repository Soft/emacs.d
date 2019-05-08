;;; init-interface.el --- Interface related settings -*- lexical-binding: t -*-

;;; Commentary:

;; Interface related settings.

;;; Code:

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

(adq/if-supported tool-bar-mode -1)
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
  :ensure t
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (warn "Required fonts are missing. Remember to run `all-the-icons-install-fonts'")))

(defun adq/icon-string (set icon)
  "Return string containing `icon' from `set'."
  (-let (((getter . family)
          (pcase set
            ('alltheicon '(all-the-icons-alltheicon . all-the-icons-alltheicon-family))
            ('fileicon '(all-the-icons-fileicon . all-the-icons-fileicon-family))
            ('faicon '(all-the-icons-faicon . all-the-icons-faicon-family))
            ('octicon '(all-the-icons-octicon . all-the-icons-octicon-family))
            ('wicon '(all-the-icons-wicon . all-the-icons-wicon-family))
            ('material '(all-the-icons-material . all-the-icons-material-family)))))
    (propertize (funcall getter icon)
                'face `(:family ,(funcall family) :height 1.2)
                'display '(raise -0.1))))

(memoize 'adq/icon-string)

(use-package page-break-lines
  :disabled
  :ensure t
  :diminish page-break-lines-mode
  :init
  (setq page-break-lines-char ?·)
  (global-page-break-lines-mode))

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

(defun adq/set-frame-alpha (number)
  (interactive "nAlpha: ")
  (if (<= 5 number 100)
      (set-frame-parameter nil 'alpha number)
    (error "Invalid alpha")))

(defvar adq/frame-alpha-step 5
  "Step for frame alpha changes.")

(defun adq/frame-alpha-alter (fn)
  (adq/set-frame-alpha
   (adq/clamp 5 100
          (funcall fn (or (frame-parameter nil 'alpha) 100)))))

(defun adq/frame-alpha-inc ()
  "Increase frame alpha by `adq/frame-alpha-step'."
  (interactive)
  (adq/frame-alpha-alter (lambda (value) (+ value adq/frame-alpha-step))))

(defun adq/frame-alpha-dec ()
  "Decrease frame alpha by `adq/frame-alpha-step'."
  (interactive)
  (adq/frame-alpha-alter (lambda (value) (- value adq/frame-alpha-step))))

(defhydra adq/hydra-frame-alpha nil
  "Set frame opacity"
  ("+" adq/frame-alpha-inc)
  ("-" adq/frame-alpha-dec))

(defun adq/toggle-fullscreen ()
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "toggle-fulscreen is only available on X windows systems.")))

(bind-key "<f11>" #'adq/toggle-fullscreen)

(use-package centered-window
  :ensure t
  :defer t)

(use-package volatile-highlights
  :disabled
  :ensure t
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode)
  :config
  (vhl/define-extension 'evil
                        'evil-paste-after
                        'evil-paste-before
                        'evil-paste-pop
                        'evil-move)
  (vhl/install-extension 'evil))

(defvar adq/beacon-recalibration-percent 10
  "How many percents should beacon color differ from the
  background color.")

(use-package dimmer
  :ensure t
  :init
  (dimmer-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (beacon-mode)
  :config
  (adq/add-to-list-many
   'beacon-dont-blink-major-modes
   '(tabulated-list-mode
     eww-mode
     deadgrep-mode))
  ;; It would be nice if this took into account the background color of the
  ;; location where point is.
  (defun adq/beacon-recalibrate-color ()
    (setq beacon-color
          (adq/color-derive adq/beacon-recalibration-percent
                            (frame-parameter nil 'background-color))))
  (add-hook 'adq/switch-theme-hook #'adq/beacon-recalibrate-color))

(use-package fill-column-indicator
  :ensure t
  :defer t)

(provide 'init-interface)

;;; init-interface.el ends here
