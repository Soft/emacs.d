
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package nlinum
  :ensure t
  :bind (("C-c C-l" . nlinum-mode)))

(use-package pcre2el
  :ensure t)

(use-package origami
  :ensure t)

(use-package wc-mode
  :ensure t)

(setq vc-follow-symlinks t)

(setq-default sentence-end-double-space nil
              indent-tabs-mode nil
              tab-stop-list ()
              tab-width 2)

(provide 'init-editor)
