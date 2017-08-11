;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

(defun projectile-define-root ()
  "Create .projectile file to current buffer's directory."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when file
      (write-region "" nil (concat (file-name-directory file) ".projectile")))))

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

(provide 'init-projectile)
