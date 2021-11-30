;;; config-vim.el -*- lexical-binding: t; -*-

(defvar adq/evil-disabled-modes
  '(comint-mode
    eshell-mode
    inferior-emacs-lisp-mode
    inferior-haskell-mode
    inferior-python-mode
    vterm-mode))

(use-package evil
  :init
  (evil-mode)
  :preface
  (setq evil-cross-lines t
        evil-search-wrap t
        evil-default-cursor t
        evil-want-keybinding nil ;; Evil collection requires this
        evil-want-fine-undo t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  :bind (:map evil-normal-state-map
              ("J" . evil-scroll-down)
              ("K" . evil-scroll-up)
              :map evil-visual-state-map
              ("J" . evil-scroll-down)
              ("K" . evil-scroll-up))
  :config
  (dolist (mode adq/evil-disabled-modes)
    (evil-set-initial-state mode 'emacs)))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (setq evil-collection-key-blacklist '("J" "K"))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-numbers
  :after evil
  :config
  (defhydra adq/hydra-evil-numbers (global-map "C-c")
    "Operate on the number"
    ("+" evil-numbers/inc-at-pt)
    ("-" evil-numbers/dec-at-pt)))

(use-package evil-nerd-commenter
  :after evil
  :bind
  (:map evil-normal-state-map
        ("\\" . evilnc-comment-or-uncomment-lines)
   :map evil-visual-state-map
        ("\\" . evilnc-comment-or-uncomment-lines)))

(provide 'config-vim)
