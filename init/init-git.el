;;; init-git.el --- Git settings -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :bind
  (("C-c g g" . magit-status)
   ("C-c g d" . magit-diff)))

(use-package git-timemachine
  :ensure t
  :bind
  (("C-c g t" . git-timemachine)))

(use-package gitconfig-mode
  :mode ((".gitconfig" . gitconfig-mode)
         (".gitmodules" . gitconfig-mode))
  :ensure t)

(use-package gitattributes-mode
  :mode ((".gitattributes" . gitattributes-mode))
  :ensure t)

(use-package gitignore-mode
  :mode ((".gitignore" . gitignore-mode))
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
  :config
  (require 'git-gutter-fringe)
  :bind (("C-c g f" . git-gutter-mode)))

(use-package smerge-mode
  :defer t
  :init
  (setq smerge-command-prefix "\C-cgv"))

(use-package browse-at-remote
  :ensure t
  :bind (("C-c g b" . browse-at-remote)))

(defvar git-binary "git"
  "Git binary path.")

(defun git-find-repository-root ()
  "Try to find git repository root starting from current working directory."
  (f--traverse-upwards (f-exists? (f-expand ".git" it)) (pwd)))

(defun git-repository-status (&optional root)
  "Return hash table describing git respository status."
  (let ((repository (or root
                        (git-find-repository-root)))
        (results (make-hash-table)))
    (when repository
      (cl-loop for line in (ignore-errors
                             (process-lines git-binary "-C" repository "status" "--porcelain"))
               for (status file) = (s-split-up-to " " (s-trim line) 2)
               for key = (pcase status
                           ("M" 'modified)
                           ("A" 'added)
                           ("D" 'deleted)
                           ("R" 'renamed)
                           ("C" 'copied)
                           ("??" 'untracked))
               do (let ((value (gethash key results)))
                    (puthash key (cons file value) results)) 
               finally (return results)))))

(provide 'init-git)
