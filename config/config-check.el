;;; config-check.el -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (fringe-helper-define
    'flycheck-fringe-bitmap-double-arrow nil
    "...XX..."
    "..XXXX.."
    "..XXXX.."
    "...XX..."
    "...XX..."
    "........"
    "...XX..."
    "...XX...") 
  (setq-default
   flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(use-package jinx
  :if (version<= "29.0" emacs-version)
  :hook (text-mode . jinx-mode))

(provide 'config-check)
