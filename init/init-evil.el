;;; init-evil.el --- Vim emulation -*- lexical-binding: t -*-

(setq
 evil-disabled-modes
 '(inferior-emacs-lisp-mode
   inferior-haskell-mode
   inferior-python-mode
   geiser-repl-mode
   comint-mode
   bm-show-mode
   artist-mode
   eshell-mode
   haskell-interactive-mode
   dashboard-mode
   paradox-mode
   dired-mode
   weechat-mode
   xkcd-mode
   plasma-mode
   term-mode
   neotree-mode
   helpful-mode
   doctor-mode
   sunshine-mode
   tetris-mode
   epa-list-keys
   paradox-menu-mode
   xwidget-webkit-mode
   nov-mode
   snake-mode))

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
        (mode evil-disabled-modes)
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
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :ensure t
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dev-at-pt)))

(use-package evil-nerd-commenter
  :defer t
  :ensure t)

(provide 'init-evil)
