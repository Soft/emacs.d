;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

(defun neotree-setup ()
  (setq-local mode-line-format nil))

(use-package neotree
  :ensure t
  :bind (("<f12>" . neotree-toggle))
  :init
  (add-hook 'neotree-mode-hook #'neotree-setup)
  :config
  (setq-default neo-smart-open t
                neo-vc-integration '(face char)
                neo-force-change-root t)
  (bind-keys
   :map neotree-mode-map
   ("j" . neotree-next-line)
   ("k" . neotree-previous-line))
  (after-load 'projectile
    (setq projectile-switch-project-action #'neotree-projectile-action)))

(provide 'init-neotree)
