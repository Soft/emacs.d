;;; init-evil.el --- Vim emulation -*- lexical-binding: t -*-

;;; Commentary:

;; I was a long time Vim user before and really like the modal editing paradigm.

;;; Code:

(defvar evil-disabled-modes
  '(inferior-emacs-lisp-mode
    inferior-haskell-mode
    inferior-python-mode
    geiser-repl-mode
    comint-mode
    bm-show-mode
    artist-mode
    eshell-mode
    haskell-interactive-mode
    life
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
    snake-mode)
  "Modes where Evil mode should not be used.")

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
     ("\\" . comment-or-uncomment-region))

    (bind-keys
     :map evil-insert-state-map
     ("C-n" . company-complete))))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :ensure t
  :defer t)

(defhydra hydra-evil-numbers (global-map "C-c")
  "Operate on the number"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt))

(use-package evil-nerd-commenter
  :defer t
  :ensure t)

(provide 'init-evil)

;;; init-evil.el ends here
