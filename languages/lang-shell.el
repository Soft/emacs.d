;; -*- mode: Emacs-Lisp; lexical-binding: t; coding: utf-8 -*-
;; Shell

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'lang-shell)
