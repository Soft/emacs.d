;;; config-search.el -*- lexical-binding: t; -*-

(use-package deadgrep
  :defer t)

(use-package google-this
  :bind (("C-c f" . google-this)))

(provide 'config-search)
