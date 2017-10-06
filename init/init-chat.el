;;; init-chat.el --- Chat configuration -*- lexical-binding: t -*-

(use-package weechat
  :defer t
  :ensure t
  :config
  (!cons 'weechat-notifications weechat-modules)
  (set-face-attribute 'weechat-time-face nil :foreground "light grey")
  (bind-keys
   :map weechat-mode-map
   ("C-c SPC" . helm-weechat-buffers))
  (setq weechat-time-format "%H:%M"
        weechat-text-column 0
        weechat-header-line-format "%t"
        weechat-auto-monitor-buffers t
        weechat-color-list '("dark green" "dark red" "dark orange"
                             "royal blue" "dark magenta" "purple"
                             "yellow green" "dodger blue" "violet"
                             "goldenrod" "tomato" "red1"
                             "SpringGreen1" "gold" "lime green"
                             "VioletRed2" "OliveDrab4" "maroon1"
                             "turquoise" "orchid" "lawn green")))

(defun weechat-read-relay-password ()
  "Prompt for relay password."
  (read-passwd "Relay password: "))

(defvar weechat-password-default #'weechat-read-relay-password
  "Default relay password for weechat-connect-default.")

(defun weechat-connect-default ()
  "Connect to weechat with defautl settings."
  (interactive)
  (weechat-connect weechat-host-default weechat-port-default weechat-password-default))

(use-package helm-weechat
  :commands (helm-weechat-buffers)
  :bind (("C-x c w" . helm-weechat-buffers)))

(provide 'init-chat)
