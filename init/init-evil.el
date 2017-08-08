
(use-package evil
  :ensure t
  :init (evil-mode)
  :config
    (progn
      (setq
        evil-cross-lines t
        evil-default-cursor t)
      (dolist (mode '(inferior-emacs-lisp-mode
                      inferior-haskell-mode
                      inferior-python-mode
                      geiser-repl-mode
                      comint-mode
                      artist-mode
                      eshell-mode
                      haskell-interactive-mode
                      dired-mode
                      term-mode
                      neotree-mode
                      doctor-mode
                      tetris-mode
                      epa-list-keys
                      xwidget-webkit-mode
                      snake-mode))
              (evil-set-initial-state mode 'emacs))))

(provide 'init-evil)