;;; config-modeline.el -*- lexical-binding: t; -*-

(use-package doom-modeline
  :diminish doom-modeline-mode
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-height 42
        doom-modeline-project-detection 'projectile))

(provide 'config-modeline)
