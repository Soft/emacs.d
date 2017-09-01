;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Shell

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'lang-shell)
