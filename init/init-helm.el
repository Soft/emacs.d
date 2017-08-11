;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Helm configuration

(use-package helm 
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (helm-mode)
  :config
  (setq helm-buffer-fuzzy-matching  t
        helm-recent-fuzzy-matching  t
        helm-M-x-fuzzy-matching     t
        helm-split-window-in-side-p t
        helm-ff-skip-boring-files   t
        helm-quick-update           t)
  (bind-keys
   :map helm-map
   ("<escape>" . helm-keyboard-quit))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-x /"   . helm-occur)
   ("C-c SPC" . helm-imenu)
   ("M-x"     . helm-M-x)
   ("C-h a"   . helm-apropos)
   ("C-h i"   . helm-info-emacs)))

(use-package helm-ag
  :if (programs-p "ag")
  :ensure t)

;; TODO: This doesn't seem to be necessary anymore
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

(use-package helm-swoop
  :ensure t)

(use-package helm-themes
  :ensure t)

(use-package helm-emoji
  :bind (("C-x c E" . helm-emoji)))

(provide 'init-helm)
