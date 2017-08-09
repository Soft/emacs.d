
(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)))

(use-package git-timemachine
  :defer t
  :ensure t)

(use-package smerge-mode
  :defer t
  :init
  (setq smerge-command-prefix "\C-cv"))

(provide 'init-git)
