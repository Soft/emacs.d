;;; config-editor.el -*- lexical-binding: t; -*-

(setq-default
 ad-redefinition-accept 'accept
 sentence-end-double-space nil
 indent-tabs-mode nil
 tab-stop-list nil
 tab-width 2
 fill-column 80
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 scroll-margin 3
 scroll-preserve-screen-position t
 save-interprogram-paste-before-kill t
 kill-ring-max 250
 tramp-default-method "ssh"
 history-delete-duplicates t
 vc-follow-symlinks t
 confirm-kill-emacs #'y-or-n-p
 confirm-kill-processes nil
 find-file-visit-truename t
 backward-delete-char-untabify-method 'hungry
 completion-styles '(flex))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :diminish dtrt-indent-mode
  :config
  (setq dtrt-indent-verbosity 0))

(use-package apheleia
  :defer t
  :init
  (defalias 'autoformat-mode 'apheleia-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package tree-sitter
  :defer 1
  :config
  (let ((inhibit-message t))
    (use-package tree-sitter-langs)
    (global-tree-sitter-mode))
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators
             :type git
             :host github
             :repo "emacs-tree-sitter/ts-fold")
  :defer t
  :init
  (setq ts-fold-indicators-face-function
        (lambda (&rest _)
          'line-number-current-line))
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode)
  :config
  (defhydra adq/hydra-fold nil
    "
_f_: Toggle Fold  _o_: Open All Folds  _c_: Close All Folds
"
    ("f" ts-fold-toggle)
    ("o" ts-fold-open-all)
    ("c" ts-fold-close-all))
  (bind-key "C-c f" #'adq/hydra-fold/body))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package rainbow-identifiers
  :hook (prog-mode . rainbow-identifiers-mode)
  :diminish rainbow-identifiers-mode)

(use-package highlight-indent-guides
  :defer t
  :diminish highlight-indent-guides-mode
  :config
  (setq-default highlight-indent-guides-method 'bitmap))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode)

(use-package paren
  :straight nil
  :diminish show-paren-mode
  :hook (prog-mode . show-paren-mode))

(use-package hl-todo
  :hook ((text-mode prog-mode) . hl-todo-mode)
  :diminish hl-todo-mode)

(use-package sudo-edit :defer t)

(use-package unfill
  :bind
  (("M-Q" . unfill-toggle)))

(use-package compile
  :straight nil
  :defer t
  :config
  (setq compilation-scroll-output t))

(use-package bury-successful-compilation
  :after compile
  :config
  (bury-successful-compilation))

(use-package expand-region
 :init
 (with-eval-after-load 'evil
   (evil-define-key 'normal prog-mode-map
     (kbd "<RET>")  #'er/expand-region)))

(use-package uniquify
  :defer 2
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator " • "))

(adq/use-local-package prettify-quotes :defer t)

(bind-keys
 ("C-w" . backward-kill-word)
 ("<escape>" . keyboard-quit)
 ("C-x k" . kill-current-buffer))

(defun adq/prog-mode-setup ()
  "Defaults for `prog-mode' buffers."
  (setq-local display-line-numbers t))

(use-package prog-mode
  :straight nil
  :init
  (global-prettify-symbols-mode)
  :config
  (add-hook 'prog-mode-hook #'adq/prog-mode-setup)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'config-editor)
