;;; init-misc.el --- Miscellaneous packages -*- lexical-binding: t -*-

;;; Commentary:

;; Miscellaneous packages.

;;; Code:

(defvar adq/opener "xdg-open"
  "Application used for opening files based on their type.")

(defun adq/open-external ()
  "Open file with an external application."
  (interactive)
  (if-let (file (cond ((eq major-mode 'dired-mode)
                       (ignore-errors (dired-get-file-for-visit)))
                      ((buffer-file-name) (buffer-file-name))
                      (t (read-file-name "File: " nil nil t))))
      (call-process adq/opener nil 0 nil file)
    (error "No file to open")))

(use-package xkcd
  :ensure t
  :commands (xkcd-rand xkcd-get-latest xkcd-get)
  :defer t
  :preface
  (add-to-list 'recentf-exclude
               "\\.emacs\\.d/xkcd/")
  :init
  (bind-key
   "C-c x X"
   (adq/switch-command #'xkcd-get-latest
                       #'xkcd-rand))
  :config
  (bind-keys
   :map xkcd-mode-map
   ("h" . xkcd-prev)
   ("l" . xkcd-next)
   ("k" . xkcd-prev)
   ("j" . xkcd-next)
   ("g" . xkcd-rand)
   ("q" . xkcd-kill-buffer)
   ("<" . (lambda ()
            (interactive)
            (xkcd-get 1)))
   (">" . xkcd-get-latest)
   ("<escape>" . xkcd-kill-buffer)))


;; FIXME: Telephone-line compatability
(use-package nyan-mode
  :disabled
  :ensure t
  :init
  ;; It's Friday!
  (when (eq (string-to-int (format-time-string "%u")) 5)
    (nyan-mode)
    (nyan-start-animation)
    (nyan-toggle-wavy-trail)))

(use-package uuidgen
  :ensure t
  :defer t)

(use-package dpaste
  :defer t
  :ensure t)

;; Remember to set sunshine-appid & sunshine-location
(use-package sunshine
  :defer t
  :ensure t
  :bind (("C-c x f" . sunshine-forecast))
  :config
  (setq sunshine-units 'metric
        sunshine-show-icons t))

;; It would be nice to separate Finnish specific things
(use-package google-translate
  :defer t
  :ensure t
  :init
  (setq google-translate-default-source-language "fi"
        google-translate-default-target-language "en"
        google-translate-output-destination 'echo-area))

(use-package google-translate-repl
  :commands (google-translate-repl)
  :bind (("C-c x T" . google-translate-repl)))

(defun adq/google-translate-with-defaults (d)
  "Query Google Translate with default languages. Reverses the
direction if universal argument is supplied."
  (interactive "P")
  (if d
      (google-translate-query-translate-reverse)
    (google-translate-query-translate)))

(bind-key "C-c x t" #'adq/google-translate-with-defaults)

(use-package define-word
  :ensure t
  :bind (("C-c x d" . define-word-at-point)))

(use-package google-this
  :ensure t
  :bind-keymap (("C-c x g" . google-this-mode-submap)))

(defun adq/nov-mode-setup ()
  (adq/setq-local
      shr-width 80
      shr-use-fonts nil))

(use-package nov
  :ensure nov
  :defer t
  :mode (("\\.epub\\'" . nov-mode))
  :init
  (add-hook 'nov-mode-hook #'adq/nov-mode-setup)
  :config
  (bind-keys
   :map nov-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . nov-scroll-up)
   ("K" . nov-scroll-down)))

(use-package mpris-control
  :commands (mpris-control-info-mode))

(use-package zenity-color-picker
  :if (adq/programs-p "zenity")
  :ensure t
  :defer t)

(use-package kurecolor
  :ensure t
  :defer t)

(defhydra adq/hydra-kurecolor nil
  "
^Hue^          ^Saturation^   ^Brightness
^^^^^^-------------------------------------------
_h_: Decrease  _s_: Decrease  _b_: Decrease
_H_: Increase  _S_: Increase  _B_: Increase
"
  ("h" kurecolor-decrease-hue-by-step)
  ("H" kurecolor-increase-hue-by-step)
  ("s" kurecolor-decrease-saturation-by-step)
  ("S" kurecolor-increase-saturation-by-step)
  ("b" kurecolor-decrease-brightness-by-step)
  ("B" kurecolor-increase-brightness-by-step))

(bind-key "C-c x c" #'adq/hydra-kurecolor/body)

(use-package restclient
  :mode (("\\.restclient'" . restclient-mode))
  :ensure t)

(defun adq/restclient ()
  "Get a restclient buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*restclient*")))
    (with-current-buffer buffer
      (restclient-mode))
    (pop-to-buffer-same-window buffer)))

(defun adq/insert-wan-ip ()
  "Insert WAN IP into the current buffer."
  (interactive)
  (request "https://api.ipify.org?format=json"
           :parser 'json-read
           :sync t
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (insert (cdr (assoc 'ip data)))))
           :error (cl-function (lambda (&allow-other-keys)
                                 (error "Failed to retrieve WAN IP")))))

(use-package lorem-ipsum
  :ensure t
  :defer t)

(use-package urldecode
  :commands (urldecode-replace-region)
  :defer t)

(use-package dbus-control
  :if (and (not (daemonp)) (locate-library "dbus"))
  :commands (dbus-control-mode)
  :init (dbus-control-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (adq/add-to-list-many 'exec-path-from-shell-variables
                        '("SSH_AUTH_SOCK"
                          "SSH_AGENT_PID"
                          "GPG_AGENT_INFO"
                          "PATH"
                          "BROWSER"))
  (exec-path-from-shell-initialize))

(use-package epa
  :defer t
  :config
  (setq epa-pinentry-mode 'loopback)
  :bind (:map epa-key-list-mode-map
              ("<tab>" . widget-forward)
              ("<escape>" . epa-exit-buffer)
              ("q" . epa-exit-buffer)
              ("j" . next-line)
              ("k" . previous-line)))

(use-package wikipedia-peek
  :bind (("C-c x W" . wikipedia-peek))
  :commands (wikipedia-peek))

(use-package scrot
  :bind (("C-c x S" . scrot))
  :commands (scrot))

(use-package tokei
  :bind (("C-c x T" . tokei))
  :commands (tokei))

(provide 'init-misc)

;;; init-misc.el ends here
