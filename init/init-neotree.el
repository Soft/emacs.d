;;; init-neotree.el --- Neotree setup -*- lexical-binding: t -*-

;;; Commentary:

;; Neotree offers a nice sidebar with directory listing

;;; Code:

(defun neotree-setup ()
  "Defaults for neotree buffers."
  (setq-local mode-line-format nil)
  (hl-line-mode))

(use-package neotree
  :ensure t
  :bind (("<f12>" . neotree-toggle))
  :init
  (add-hook 'neotree-mode-hook #'neotree-setup)
  :config
  (setq-default neo-smart-open t
                neo-vc-integration '(face char)
                neo-force-change-root t
                neo-show-updir-line nil
                neo-theme 'icons)
  (bind-keys
   :map neotree-mode-map
   ("j" . neotree-next-line)
   ("k" . neotree-previous-line)
   ("<escape>" . neotree-hide))
  (after-load 'projectile
    (setq projectile-switch-project-action #'neotree-projectile-action)))

(provide 'init-neotree)

;;; init-neotree.el ends here
