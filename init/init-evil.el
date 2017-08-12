;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Vim emulation configuration

(use-package evil
  :ensure t
  :init (evil-mode)
  :config
  (progn
    (setq
     evil-cross-lines t
     evil-default-cursor t
     evil-mode-beyond-eol t)

    (dolist
        (mode
         '(inferior-emacs-lisp-mode
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

    (bind-keys
     :map evil-normal-state-map
     ("j" . evil-next-visual-line)
     ("k" . evil-previous-visual-line)
     ("J" . evil-scroll-down)
     ("K" . evil-scroll-up)
     ("H" . smart-backward)
     ("L" . smart-forward)
     ("\\" . evilnc-comment-or-uncomment-lines)
     ("<return>" . er/expand-region))

    (bind-keys
     :map evil-visual-state-map
     ("j" . evil-next-visual-line)
     ("k" . evil-previous-visual-line)
     ("J" . evil-scroll-down)
     ("K" . evil-scroll-up)
     ("\\" . comment-or-uncomment-region))))

(use-package evil-surround
  :ensure t)

(use-package evil-numbers
  :defer t
  :ensure t)

(use-package evil-nerd-commenter
  :defer t
  :ensure t)

(provide 'init-evil)
