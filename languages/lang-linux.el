;;; lang-linux.el --- Linux things -*- lexical-binding: t -*-

;;; Commentary:

;; Languages realted to Linux environments.

;;; Code:

(use-package ebuild-mode
  :mode (("\\.ebuild\\'" . ebuild-mode)))

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

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode))
  :ensure t)

(use-package portage-mode
  :commands (portage-mode portage-mode-accept-keywords-mode portage-mode-use-mode)
  :mode (("/package\\.use" . portage-mode-use-mode)
         ("/package\\.accept_keywords" . portage-mode-accept-keywords-mode)
         ((rx "/package." (or "env" "license" "mask" "unmask")) . portage-mode)))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :ensure t
  :config
  (add-hook 'ssh-config-mode-hook #'turn-on-font-lock))

(use-package pkgbuild-mode
  :mode (("/PKGBUILD\\'" . pkgbuild-mode))
  :ensure t)

(use-package nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode))
  :ensure t)

(provide 'lang-linux)

;;; lang-linux.el ends here
