;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*-

(defun cd-here ()
  "Change working directory to current file's location."
  (interactive)
  (cd (file-name-directory buffer-file-name)))

(defun projectile-cd ()
  (interactive)
  (when (projectile-project-p)
    (cd (projectile-project-root))))

(defun projectile-define-root ()
  "Create .projectile file to current buffer's directory."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when file
      (write-region "" nil (concat (file-name-directory file) ".projectile")))))

(defun projectile-kill-unrelated-buffers ()
  "Kill buffer that do not belong the the current project."
  (interactive)
  (let* ((name (projectile-project-name))
         (project-buffers (projectile-project-buffers))
         (unrelated (--remove
                     (or (memq it project-buffers)
                         (not (buffer-file-name it)))
                     (buffer-list))))
    (if (yes-or-no-p
         (format "Are you sure you want to kill %d buffer(s) leaving only buffers belonging to '%s'? "
                 (length unrelated) name))
        (mapc #'kill-buffer (cl-remove-if 'buffer-base-buffer unrelated)))))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (setq
   projectile-completion-system 'helm
   projectile-mode-line
   '(:eval (if (projectile-project-p)
               (format " <%s>" (projectile-project-name))
             "")))
  (bind-keys
   :map projectile-command-map
   ("C" . projectile-cd)
   ("K" . projectile-kill-unrelated-buffers)))

(use-package projectile-ripgrep
  :if (programs-p "rg")
  :defer t
  :ensure t)

(provide 'init-projectile)
