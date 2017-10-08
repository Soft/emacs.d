;;; init-customize.el --- Customize -*- lexical-binding: t -*-

;;; Commentary:

;; Store customize inside Adequate's `local' directory.

;;; Code:

(let ((path (expand-file-name "local/customize.el" init-directory)))
  (unless (file-readable-p path)
    (write-region "" nil path))
  (setq custom-file path))

;; FIXME: Maybe we should let load-local-files load this
(load custom-file)

(provide 'init-customize)

;;; init-customize.el ends here
