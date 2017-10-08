;;; init-editor.el --- Editor settings -*- lexical-binding: t -*-

;;; Commentary:

;; Basic editing configuration.

;;; Code:

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default
 ad-redefinition-action 'accept
 sentence-end-double-space nil
 indent-tabs-mode nil
 tab-stop-list ()
 tab-width 2
 fill-column 80
 scroll-margin 3
 scroll-preserve-screen-position t
 save-interprogram-paste-before-kill t
 tramp-default-method "ssh"
 vc-follow-symlinks t)

(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

(put 'upcase-region 'disabled nil)

(bind-keys
 ("C-w" . backward-kill-word)
 ("<escape>" . keyboard-quit))

;; FIXME: This can be called before the use-package has installed all the
;; required packages. This can break the first setup.
(defun prog-mode-setup ()
  "Defaults for programming modes."
  (nlinum-mode)
  (global-prettify-symbols-mode)
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (fic-mode)
  (origami-mode)
  (dtrt-indent-mode)
  (flycheck-mode) 
  (company-mode)
  (company-quickhelp-mode)
  (company-statistics-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator " • "))

(use-package nlinum
  :ensure t
  :bind (("C-c l" . nlinum-mode)))

(use-package wc-mode
  :defer t
  :ensure t
  :config
  (setq wc-modeline-format "#%tw"))

(use-package fic-mode
  :defer t
  :ensure t)

(use-package subword
  :diminish superword-mode
  :init
  (global-subword-mode)
  (global-superword-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package rainbow-identifiers
  :ensure t
  :defer t)

(use-package with-editor
  :commands (with-editor-export-editor)
  :ensure t)

(use-package face-remap
  :defer t
  :diminish buffer-face-mode)

(use-package unfill
  :ensure t
  :bind (("M-Q". unfill-toggle)))

(use-package visual-fill-column
  :ensure t
  :defer t)

(use-package dtrt-indent
  :ensure t
  :defer t
  :config
  (setq dtrt-indent-active-mode-line-info "↹ "))

(use-package writegood-mode
  :ensure t
  :defer t
  :diminish writegood-mode)

(use-package typo
  :ensure t
  :defer t
  :diminish typo-mode
  :init
  (setq-default typo-language "English"))

(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'prog-mode-setup))

(defun text-mode-setup ()
  "Defaults for text modes."
  (gll-guess-language-lite-mode)
  (wc-mode))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'text-mode-setup))

(use-package recompile-on-save
  :ensure t
  :defer t)

(use-package autorevert
  :defer t
  :config
  (setq auto-revert-mode-text " 🅡"))

(provide 'init-editor)

;;; init-editor.el ends here
