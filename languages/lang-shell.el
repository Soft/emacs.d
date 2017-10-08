;;; lang-shell.el --- Shells -*- lexical-binding: t -*-

;;; Commentary:

;; Shell scripting configuration

;;; Code:

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'lang-shell)

;;; lang-shell.el ends here
