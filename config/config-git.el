;;; config-git.el -*- lexical-binding: t; -*-

(use-package magit
  :bind
  (("C-c g RET" . magit-dispatch)
   ("C-c g r" . magit-list-repositories)
   ("C-c g g" . magit-status)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-buffer-file)
   :map magit-status-mode-map
   ("<escape>" . magit-mode-bury-buffer))) 

(use-package git-timemachine
  :bind
  (("C-c g t" . git-timemachine)))

(use-package git-modes
  :mode ((".gitconfig" . gitconfig-mode)
         (".gitmodules" . gitconfig-mode)
         (".gitattributes" . gitattributes-mode)
         (".gitignore" . gitignore-mode)))
  
(provide 'config-git)
