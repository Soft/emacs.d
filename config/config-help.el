;; config-help.el -*- lexical-binding: t; -*-

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h c" . helpful-command)
   ("C-h M" . helpful-macro)
   ("C-h h" . helpful-at-point))
  :config
  (setq helpful-max-buffers 1)) 

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-c g" '("git" . "Version Control")
    "C-c p" '("project" . "Project Management")
    "C-c l" '("lsp" . "Language Server Commands")))

(use-package eldoc
  :straight nil
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

(provide 'config-help)

