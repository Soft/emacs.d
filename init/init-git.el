;;; init-git.el --- Git settings -*- lexical-binding: t -*-

;;; Commentary:

;; Git configuration. Magit is the best interface to Git I have used on any
;; platform.

;;; Code:

(defun adq/magit-diff-unstaged-or-staged (d)
  "Display unstaged changes or, if universal argument is suplied,
staged changes."
  (interactive "P")
  (if d
      (magit-diff-staged)
    (magit-diff-unstaged)))

(use-package magit
  :ensure t
  :bind
  (("C-c g g" . magit-status)
   ("C-c g p" . magit-dispatch-popup)
   ("C-c g d" . adq/magit-diff-unstaged-or-staged)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-buffer-file))
  :config
  (bind-keys
   :map magit-popup-mode-map
   ("<escape>" . magit-popup-quit)
   :map magit-status-mode-map
   ("<escape>" . magit-mode-bury-buffer)))

(defun adq/magit-find-project ()
  "Open Magit status buffer for a project."
  (interactive)
  (if-let ((projects (-filter #'adq/git-repository-root-p
                              (projectile-relevant-known-projects))))
      (projectile-completing-read
       "Magit project: " projects
       :action #'magit-status)
    (user-error "There are no known projects")))

(bind-key "C-c g P" #'adq/magit-find-project)

(use-package forge
  :after magit
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind
  (("C-c g t" . git-timemachine))
  :config
  (adq/after-load 'evil
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package git-messenger
  :ensure t
  :defer t
  :bind
  (("C-c g m" . git-messenger:popup-message))
  :config
  (setq git-messenger:use-magit-popup t))

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
  (defhydra adq/hydra-smerge nil
    "
^Move^             ^Keep^            ^Diff^                ^Action^
^^^^----------------------------------------------------------------------------
_n_: Next          _a_: All          _L_: Base/lower       _e_: Ediff
_p_: Previous      _b_: Base         _U_: Base/upper       _r_: Auto resolve
                 ^^_l_: Lower        _B_: Upper/lower      _c_: Combine
                 ^^_u_: Upper                            ^^_k_: Kill current
                 ^^_RET_: Current
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("l" smerge-keep-lower)
    ("u" smerge-keep-upper)
    ("RET" smerge-keep-current)
    ("L" smerge-diff-base-lower)
    ("U" smerge-diff-base-upper)
    ("B" smerge-diff-upper-lower)
    ("e" smerge-ediff)
    ("r" smerge-resolve)
    ("c" smerge-combine-with-next)
    ("k" smerge-kill-current))
  (bind-key "C-c g s" #'adq/hydra-smerge/body)
  (setq smerge-command-prefix "\C-cgv"))



(use-package browse-at-remote
  :ensure t
  :bind (("C-c g b" . browse-at-remote)))

(use-package github-clone
  :ensure t
  :bind (("C-c g c" . github-clone)))

(defvar adq/git-binary "git"
  "Git binary path.")

(defun adq/git-repository-root-p (root)
  "Check if path `root' is a git repository root."
  (f-exists? (f-join root ".git")))

(defun adq/git-find-repository-root ()
  "Try to find git repository root starting from current working directory."
  (f--traverse-upwards (f-exists? (f-expand ".git" it)) (pwd)))

(defun adq/git-repository-status (&optional root)
  "Return hash table describing git respository status."
  (let ((repository (or root
                        (adq/git-find-repository-root)))
        (results (make-hash-table)))
    (when repository
      (cl-loop for line in (ignore-errors
                             (process-lines adq/git-binary "-C" repository "status" "--porcelain"))
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

(defun adq/git-current-branch (&optional root)
  "Return current git branch as a string."
  (let ((repository (or root
                        (adq/git-find-repository-root))))
    (when repository
      (let ((output
             (ignore-errors
               (process-lines adq/git-binary "-C" repository "rev-parse" "--symbolic-full-name" "--abbrev-ref" "HEAD"))))
        (car output)))))

(provide 'init-git)

;;; init-git.el ends here
