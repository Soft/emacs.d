;;; config-bookmarks.el -*- lexical-binding: t; -*-

(use-package bookmark
  :straight nil
  :config
  (setq bookmark-save-flag 1)
  :bind-keymap
  ("C-c m" . bookmark-map))

(provide 'config-bookmarks)
