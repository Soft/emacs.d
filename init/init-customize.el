;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Control customize

(let ((path (expand-file-name "local/customize.el" init-directory)))
  (unless (file-readable-p path)
    (write-region "" nil path))
  (setq custom-file path))

;; FIXME: Maybe we should let load-local-files load this
(load custom-file)

(provide 'init-customize)
