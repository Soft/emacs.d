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

(provide 'config-check)
