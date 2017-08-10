;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Editor settings

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
  :ensure t
  :diminish pcre-mode
  :init (pcre-mode))

(use-package origami
  :defer t
  :ensure t)

(use-package wc-mode
  :defer t
  :ensure t)

(use-package fic-mode
  :defer t
  :ensure t)

(use-package subword
  :diminish superword-mode
  :init
  (global-subword-mode)
  (global-superword-mode))

(setq vc-follow-symlinks t)

(setq-default
 sentence-end-double-space nil
 indent-tabs-mode nil
 tab-stop-list ()
 tab-width 2
 fill-column 80
 scroll-margin 3
 scroll-preserve-screen-position t
 save-interprogram-paste-before-kill t)

(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package rainbow-identifiers
  :ensure t
  :defer t)

(use-package unfill
  :ensure t
  :bind (("M-Q". unfill-toggle)))

(use-package guess-language
  :ensure t
  :defer t
  :init
  (setq guess-language-langueages '(en fi)
        guess-language-min-paragraph-length 35))

(use-package ag
  :if (programs-p "ag")
  :defer t
  :ensure t)

(use-package writegood-mode
  :ensure t
  :defer t)

(defun prog-mode-setup ()
  "Defaults for programming modes."
  (global-prettify-symbols-mode)
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (fic-mode)
  (origami-mode)
  (company-mode)
  (company-quickhelp-mode)
  (company-statistics-mode))

(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'prog-mode-setup))

(defun text-mode-setup ()
  "Defaults for text modes."
  (guess-language-mode)
  (writegood-mode)
  (wc-mode))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'text-mode-setup))

(bind-keys
 ("C-w" . backward-kill-word)
 ("<escape>" . keyboard-quit))

(provide 'init-editor)
