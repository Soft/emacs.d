
(use-package magit
  :ensure t
  :bind
  (("C-c g g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind
  (("C-c g t" . git-timemachine)))

(use-package gitconfig-mode
  :mode ((".gitconfig" . gitconfig-mode)
         (".gitmodules" . gitconfig-mode))
  :defer t
  :ensure t)

(use-package gitattributes-mode
  :mode ((".gitattributes" . gitattributes-mode))
  :defer t
  :ensure t)

(use-package gitignore-mode
  :mode ((".gitignore" . gitignore-mode))
  :defer t
  :ensure t)

(use-package smerge-mode
  :defer t
  :init
  (setq smerge-command-prefix "\C-cgv"))

(provide 'init-git)
