;;; lang-shell.el --- Shells -*- lexical-binding: t -*-

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'lang-shell)
