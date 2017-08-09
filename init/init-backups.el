;;; Backups

(let ((backup-path (f-join user-emacs-directory "backups"))
      (auto-save-path (f-join user-emacs-directory "saves")))
  (unless (f-directory? backup-path)
    (f-mkdir backup-path))
  (unless (f-directory? auto-save-path)
    (f-mkdir auto-save-path))
  (setq-default backup-directory-alist `((".*" . ,(f-slash backup-path)))
                auto-save-file-name-transforms `((".*" ,(f-slash auto-save-path) t))))

(setq-default backup-by-copying t
              delete-old-versions t
              version-control t
              vc-make-backup-files t)

(provide 'init-backups)
