;; -*- lexical-binding: t -*-
;; Miscellaneous languages

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :ensure t)

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :ensure t)

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :ensure t)

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter "ruby"
  :ensure t)

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :interpreter ("php" . php-mode)
  :ensure t)

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :interpreter ("scala" . scala-mode)
  :ensure t)

(use-package ebuild-mode
  :mode (("\\.ebuild\\'" . ebuild-mode)))

(use-package qml-mode
  :mode (("\\.qml\\'" . qml-mode))
  :interpreter ("qml" . qml-mode)
  :ensure t)

(use-package vimrc-mode
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode))
  :ensure t)

(use-package systemd
  :ensure t
  :mode ((rx "."
             (or "automount"
                 "busname"
                 "mount"
                 "service"
                 "slice"
                 "socket"
                 "swap"
                 "target"
                 "timer"
                 "link"
                 "netdev"
                 "network")
             string-end)
         . systemd-mode))

(use-package ssh-config-mode
  :ensure t
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :config
  (add-hook 'ssh-config-mode-hook #'turn-on-font-lock))

(provide 'lang-misc)
