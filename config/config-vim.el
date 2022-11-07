;;; config-vim.el -*- lexical-binding: t; -*-

(defvar adq/evil-disabled-modes
  '(comint-mode
    eshell-mode
    inferior-emacs-lisp-mode
    inferior-haskell-mode
    inferior-python-mode
    cider-repl-mode
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
              ("<remap> <evil-next-line>" . evil-next-visual-line)
              ("<remap> <evil-previous-line>" . evil-previous-visual-line)
              ("J" . evil-scroll-down)
              ("K" . evil-scroll-up)
              :map evil-visual-state-map
              ("<remap> <evil-next-line>" . evil-next-visual-line)
              ("<remap> <evil-previous-line>" . evil-previous-visual-line)
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

(use-package evil-textobj-tree-sitter
  :after (tree-sitter evil)
  :config
  (bind-key "f"
            (evil-textobj-tree-sitter-get-textobj "function.outer")
            evil-outer-text-objects-map)
  (bind-key "f"
            (evil-textobj-tree-sitter-get-textobj "function.inner")
            evil-inner-text-objects-map)
  (bind-key "b"
            (evil-textobj-tree-sitter-get-textobj "block.outer")
            evil-outer-text-objects-map)
  (bind-key "b"
            (evil-textobj-tree-sitter-get-textobj "block.inner")
            evil-inner-text-objects-map)
  (bind-key "c"
            (evil-textobj-tree-sitter-get-textobj "conditional.outer")
            evil-outer-text-objects-map)
  (bind-key "c"
            (evil-textobj-tree-sitter-get-textobj "conditional.inner")
            evil-inner-text-objects-map)
  (bind-key "l"
            (evil-textobj-tree-sitter-get-textobj "loop.outer")
            evil-outer-text-objects-map)
  (bind-key "l"
            (evil-textobj-tree-sitter-get-textobj "loop.inner")
            evil-inner-text-objects-map)
  (bind-key "a"
            (evil-textobj-tree-sitter-get-textobj "call.outer")
            evil-outer-text-objects-map)
  (bind-key "a"
            (evil-textobj-tree-sitter-get-textobj "call.inner")
            evil-inner-text-objects-map)
  (bind-key "P"
            (evil-textobj-tree-sitter-get-textobj "parameter.outer")
            evil-outer-text-objects-map)
  (bind-key "P"
            (evil-textobj-tree-sitter-get-textobj "parameter.inner")
            evil-inner-text-objects-map)
  (bind-key "C"
            (evil-textobj-tree-sitter-get-textobj "class.outer")
            evil-outer-text-objects-map)
  (bind-key "C"
            (evil-textobj-tree-sitter-get-textobj "class.inner")
            evil-inner-text-objects-map)
  (bind-key "s"
            (evil-textobj-tree-sitter-get-textobj "statement.outer")
            evil-outer-text-objects-map))

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

(use-package evil-matchit
  :after evil
  :diminish global-evil-matchit-mode
  :config
  (global-evil-matchit-mode))

(use-package evil-nerd-commenter
  :after evil
  :bind
  (:map evil-normal-state-map
        ("\\" . evilnc-comment-or-uncomment-lines)
        :map evil-visual-state-map
        ("\\" . evilnc-comment-or-uncomment-lines)))

(provide 'config-vim)
