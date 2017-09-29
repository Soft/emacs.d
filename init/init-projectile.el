;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*-

(defun cd-here ()
  "Change working directory to current file's location."
  (interactive)
  (cd (file-name-directory buffer-file-name)))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (setq
   projectile-completion-system 'helm
   projectile-mode-line
   '(:eval (if (projectile-project-p)
               (format " <%s>" (projectile-project-name))
             ""))))

(defun projectile-cd ()
  (interactive)
  (when (projectile-project-p)
    (cd (projectile-project-root))))

(bind-key "C-c p C" #'projectile-cd)

(defun projectile-define-root ()
  "Create .projectile file to current buffer's directory."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when file
      (write-region "" nil (concat (file-name-directory file) ".projectile")))))

(use-package projectile-ripgrep
  :if (programs-p "rg")
  :defer t
  :ensure t)

(provide 'init-projectile)
