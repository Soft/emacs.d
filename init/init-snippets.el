;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Use yasnippet for snippets

;;; Code:

(use-package yasnippet
  :ensure t
  :defer  t
  :diminish yas-minor-mode
  :init
  (setq yas-verbosity 2)
  :config
  (add-to-list
   'yas-snippet-dirs
   (f-join init-directory "resources" "snippets") t))

(provide 'init-snippets)

;;; init-snippets.el ends here
