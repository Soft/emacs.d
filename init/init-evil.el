
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
      (evil-set-initial-state mode 'emacs))

    (bind-keys :map evil-normal-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("J" . evil-scroll-down)
               ("K" . evil-scroll-up))

    (bind-keys :map evil-visual-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("J" . evil-scroll-down)
               ("K" . evil-scroll-up))))

(provide 'init-evil)
