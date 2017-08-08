
(use-package helm
  :ensure t
  :diminish helm-mode
  :init (require 'helm-config)
  :config
    (setq helm-buffer-fuzzy-matching  t
          helm-recent-fuzzy-matching  t
          helm-M-x-fuzzy-matching     t
          helm-split-window-in-side-p t
          helm-ff-skip-boring-files   t
          helm-quick-update           t)
  :bind
    (("C-x C-f" . helm-find-files)
     ("C-x b"   . helm-mini)
     ("C-x C-b" . helm-buffers-list)
     ("C-x /"   . helm-occur)
     ("C-c SPC" . helm-imenu)
     ("M-x"     . helm-M-x)
     ("C-h a"   . helm-apropos)
     ("C-h i"   . helm-info-emacs)))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on)
  :bind
    (("C-x f" . helm-projectile-find-file)
     ("C-x g" . helm-projectile-ag)))

(use-package helm-descbinds
  :ensure t
  :bind
    (("C-h b" . helm-descbinds)))

(provide 'init-helm)