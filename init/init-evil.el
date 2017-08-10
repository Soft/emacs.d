;;; Vim emulation configuration

(use-package evil
  :ensure t
  :init (evil-mode)
  :config
  (progn
    (setq
     evil-cross-lines t
     evil-default-cursor t
     evil-mode-beyond-eol t)

    (dolist (mode '(inferior-emacs-lisp-mode
                    inferior-haskell-mode
                    inferior-python-mode
                    geiser-repl-mode
                    comint-mode
                    artist-mode
                    eshell-mode
                    haskell-interactive-mode
                    dashboard-mode
                    paradox-mode
                    dired-mode
                    weechat-mode
                    xkcd-mode
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
               ("K" . evil-scroll-up)
               ("<return>" . er/expand-region))

    (bind-keys :map evil-visual-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("J" . evil-scroll-down)
               ("K" . evil-scroll-up))))

(use-package evil-surround
  :ensure t)

(use-package evil-numbers
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t)

(provide 'init-evil)
