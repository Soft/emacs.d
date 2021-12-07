;;; config-interface.el -*- lexical-binding: t; -*-

(setq
 inhibit-splash-screen t
 inhibit-startup-message t)

(setq-default
 line-spacing 1
 frame-resize-pixelwise t
 inhibit-x-resources t
 use-dialog-box nil
 use-file-dialog nil
 x-gtk-use-system-tooltips nil
 x-underline-at-descent-line t
 echo-keystrokes 0.1
 xterm-mouse-mode 1
 cursor-in-non-selected-windows nil
 mouse-yank-at-point t
 ring-bell-function 'ignore
 fast-but-imprecise-scrolling t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 select-enable-clipboard t
 select-enable-primary t)

(setq frame-title-format
      '((:eval (concat
                (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b")
                " - emacs@"
                system-name))))

(fringe-mode '(8 . 8))

(defun adq/toggle-pseudo-fullscreen ()
  "Toggle pseudo-fullscreen mode for the current frame."
  (interactive)
  (if (and (eq (frame-parameter nil 'fullscreen) 'maximized)
           (frame-parameter nil 'undecorated))
      (progn
        (set-frame-parameter nil 'fullscreen nil)
        (set-frame-parameter nil 'undecorated nil))
    (progn
      (set-frame-parameter nil 'fullscreen 'maximized)
      (set-frame-parameter nil 'undecorated t))))

(use-package volatile-highlights
  :hook (prog-mode . volatile-highlights-mode)
  :diminish volatile-highlights-mode
  :config
  (with-eval-after-load 'evil
    (vhl/define-extension 'evil
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop
                          'evil-move)
    (vhl/install-extension 'evil)))

(provide 'config-interface)
