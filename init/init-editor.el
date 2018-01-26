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
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 scroll-margin 3
 scroll-preserve-screen-position t
 save-interprogram-paste-before-kill t
 tramp-default-method "ssh"
 vc-follow-symlinks t)

(diminish 'visual-line-mode " ⓥ")

;; Enabled narrow
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

(put 'upcase-region 'disabled nil)

(bind-keys
 ("C-w" . backward-kill-word)
 ("<escape>" . keyboard-quit))

(defun adq/where-am-i ()
  (interactive)
  (message "%s%s%sline: %d; column: %d"
           (let ((path (or (buffer-file-name) (buffer-name))))
             (format "%s; " path))
           (if (projectile-project-p)
               (format "%s; " (projectile-project-name)) "") 
           (if-let ((branch (adq/git-current-branch)))
               (format "%s; " branch) "")
           (line-number-at-pos)
           (current-column)))

(bind-key "C-c x W" #'adq/where-am-i)

;; I regularly work on projects with large TAGS files
(advice-add
 'abort-if-file-too-large :around
 (lambda (fn size op-type filename)
   (unless (equal (f-filename filename) "TAGS")
     (apply fn (list size op-type filename)))))

;; FIXME:
;; This could be called before the use-package has installed all the
;; required packages. This can break the first setup.
(defun adq/prog-mode-setup ()
  "Defaults for programming modes."
  (nlinum-mode)
  (global-prettify-symbols-mode)
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (fic-mode)
  (origami-mode)
  (dtrt-indent-mode)
  (flycheck-mode)
  (yas-minor-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator " • "))

(use-package nlinum
  :ensure t
  :defer t)

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

(use-package whitespace
  :defer t
  :diminish " ⓦ")

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

(use-package highlight-indent-guides
  :ensure t
  :defer t)

(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'adq/prog-mode-setup))

(defun adq/text-mode-setup ()
  "Defaults for text modes."
  (gll-guess-language-lite-mode)
  (wc-mode))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'adq/text-mode-setup))

(use-package recompile-on-save
  :ensure t
  :defer t)

(use-package autorevert
  :defer t
  :config
  (setq auto-revert-mode-text " 🅡"))

(defhydra adq/hydra-buffer nil
  "
^Buffer^              ^Toggle^                   ^Text Size
^^^^^^------------------------------------------------------------
_d_: Diff with file   _w_: Visible whitespace    _-_: Decrease
_r_: Revert buffer    _i_: Indent guides         _+_: Increase
                    ^^_n_: Line numbers
                    ^^_t_: Truncate lines
                    ^^_v_: Visual lines
                    ^^_a_: Auto revert
"
  ("d" magit-diff-buffer-file)
  ("r" revert-buffer)
  
  ("w" whitespace-mode)
  ("i" highlight-indent-guides-mode)
  ("n" nlinum-mode)
  ("t" toggle-truncate-lines)
  ("v" visual-line-mode)
  ("a" auto-revert-mode)
  
  ("-" text-scale-decrease)
  ("+" text-scale-increase))

(bind-key "C-c t" #'adq/hydra-buffer/body)

(provide 'init-editor)

;;; init-editor.el ends here
