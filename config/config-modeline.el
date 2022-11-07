;;; config-modeline.el -*- lexical-binding: t; -*-

(use-package doom-modeline
  :diminish doom-modeline-mode
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-height 42
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'relative-from-project))

(provide 'config-modeline)
