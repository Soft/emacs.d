;;; init-evil.el --- Vim emulation -*- lexical-binding: t -*-

;;; Commentary:

;; I was a long time Vim user before and really like the modal editing paradigm.

;;; Code:

(defvar adq/evil-disabled-modes
  '(artist-mode
    bm-show-mode
    comint-mode
    dashboard-mode
    deadgrep-mode
    dired-mode
    doctor-mode
    elfeed-search-mode
    elfeed-show-mode
    epa-info-mode
    epa-key-list-mode
    epa-key-mode
    eshell-mode
    eww-bookmark-mode
    eww-history-mode
    eww-mode
    flycheck-error-list-mode
    font-lock-studio-mode
    geiser-repl-mode
    google-translate-repl-mode
    haskell-interactive-mode
    helpful-mode
    inferior-emacs-lisp-mode
    inferior-haskell-mode
    inferior-python-mode
    life-mode
    magithub-dash-mode
    magithub-issue-view-mode
    messages-buffer-mode
    neotree-mode
    nov-mode
    paradox-menu-mode
    paradox-mode
    plasma-mode
    rtags-mode
    snake-mode
    sunshine-mode
    term-mode
    tetris-mode
    tokei-mode
    vterm-mode
    weechat-mode
    wikipedia-peek-mode
    xkcd-mode
    xref--xref-buffer-mode
    xwidget-webkit-mode)
  "Modes where Evil mode should not be used.")

(use-package evil
  :ensure t
  :init
  (evil-mode)
  :config
  (setq evil-cross-lines t
        evil-search-wrap t
        evil-default-cursor t
        evil-mode-beyond-eol t)

  (dolist
      (mode adq/evil-disabled-modes)
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
   ("\\" . comment-or-uncomment-region)))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :ensure t
  :defer t)

(use-package evil-iedit-state
  :after evil
  :demand t
  :ensure t)

(defhydra adq/hydra-evil-numbers (global-map "C-c")
  "Operate on the number"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt))

(use-package evil-nerd-commenter
  :defer t
  :ensure t)

(provide 'init-evil)

;;; init-evil.el ends here
