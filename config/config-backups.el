;;; config-backups.el -*- lexical-binding: t; -*-

(let ((backup-path (concat no-littering-var-directory "backups/"))
      (auto-save-path (concat no-littering-var-directory "saves/")))
  (unless (file-directory-p backup-path)
    (make-directory backup-path))
  (unless (file-directory-p auto-save-path)
    (make-directory auto-save-path))
  (setq-default backup-directory-alist `((".*" . ,backup-path))
                auto-save-file-name-transforms `((".*" ,auto-save-path t))))

(setq-default backup-by-copying t
              delete-old-versions t
              version-control t
              vc-make-backup-files t)

(provide 'config-backups)
