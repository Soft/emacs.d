;;; config-projects.el -*- lexical-binding: t; -*-

(use-package projectile
  :init (projectile-global-mode)
  :bind
  (("C-c c" . projectile-compile-project)
   :map projectile-mode-map
   ("C-c p" . projectile-command-map)
   :map projectile-command-map
   ("C" . adq/projectile-cd)
   ("K" . adq/projectile-kill-unrelated-buffers)))

(defun adq/cd-here ()
  "Change working directory to current file's location."
  (interactive)
  (cd (file-name-directory buffer-file-name)))

(defun adq/projectile-buffer-project ()
  (when-let* ((file-name (buffer-file-name))
              (dir (file-name-directory file-name)))
    (projectile-project-p dir)))

(defun adq/projectile-define-root ()
  "Create .projectile file to current buffer's directory."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when file
      (write-region "" nil (concat (file-name-directory file) ".projectile")))))

(defun adq/projectile-cd ()
  (interactive)
  (when (projectile-project-p)
    (cd (projectile-project-root))))

(defun adq/projectile-kill-unrelated-buffers ()
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

(defun adq/projectile-revert-buffers ()
  "Revert all project buffers."
  (interactive)
  (let ((name (projectile-project-name))
        (buffers (projectile-project-buffers)))
    (if (yes-or-no-p
         (format "Are you sure you want to revert %d buffers(s) for '%s'? "
                 (length buffers) name))
        (mapc (lambda (buffer) (with-current-buffer buffer) (revert-buffer nil t t))
              (cl-remove-if 'buffer-base-buffer buffers)))))

(provide 'config-projects)

