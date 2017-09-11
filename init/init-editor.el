;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Editor settings

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default
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
;; required packages. This can break first setup.
(defun prog-mode-setup ()
  "Defaults for programming modes."
  (nlinum-mode)
  (global-prettify-symbols-mode)
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (fic-mode)
  (origami-mode)
  (flycheck-mode)
  (company-mode)
  (company-quickhelp-mode)
  (company-statistics-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package nlinum
  :ensure t
  :bind (("C-c l" . nlinum-mode)))

(use-package origami
  :defer t
  :ensure t
  :bind
  (("C-c f o" . origami-open-node)
   ("C-c f O" . origami-open-all-nodes)
   ("C-c f c" . origami-close-node)
   ("C-c f C" . origami-close-all-nodes)
   ("C-c f 1" . origami-show-only-node)))

(defhydra hydra-origami-toggle (global-map "C-c f")
  "Recursively toggle nodes."
  ("<tab>" #'origami-recursively-toggle-node)
  ("n" #'origami-next-fold)
  ("p" #'origami-previous-fold)
  ("u" #'origami-undo)
  ("r" #'origami-redo))

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

(use-package ag
  :if (programs-p "ag")
  :defer t
  :ensure t)

(use-package writegood-mode
  :ensure t
  :defer t
  :diminish writegood-mode)

(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'prog-mode-setup))

(defun text-mode-setup ()
  "Defaults for text modes."
  (gll/guess-language-lite-mode)
  ;; (writegood-mode) ;; FIXME: Only in English
  (wc-mode))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'text-mode-setup))

(provide 'init-editor)
