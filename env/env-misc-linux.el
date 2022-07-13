;;; env-misc-linux.el -*- lexical-binding: t; -*-

(use-package vimrc-mode
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package systemd
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

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package pkgbuild-mode
  :mode (("/PKGBUILD\\'" . pkgbuild-mode)))

(use-package nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

(use-package systemtap-mode
  :mode (("\\.stp\\''" . systemtap-mode)))

(use-package dpkg-dev-el
  :mode (("/debian/control\\'" . debian-control-mode)
         ("/debian/.*copyright\\'" . debian-copyright-mode)
         ("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'" . debian-changelog-mode)))

(provide 'env-misc-linux)
