;;; init-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Search configuration. Additionally, are some search related things in
;; `init-helm.el'.

;;; Code:

(use-package pcre2el
  :ensure t
  :diminish pcre-mode
  :init (pcre-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode)
  (after-load 'evil
    (use-package evil-anzu
      :ensure t
      :init (require 'evil-anzu))))

;; FIXME: This really needs more work
(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode)

(use-package dumb-jump
  :ensure t
  ;;:config (setq dumb-jump-selector 'helm)
  :bind (("C-c d d" . dumb-jump-go)
         ("C-c d b" . dumb-jump-back)
         ("C-c d q" . dumb-jump-quick-look)))

(use-package ag
  :if (programs-p "ag")
  :defer t
  :ensure t)

(use-package ripgrep
  :if (programs-p "rg")
  :defer t
  :ensure t)

(provide 'init-search)

;;; init-search.el ends here
