
(let ((path (expand-file-name "customize.el" user-emacs-directory)))
  (unless (file-readable-p path)
    (write-region "" nil path))
  (setq custom-file path))

(load custom-file)

(provide 'init-customize)