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
 kill-ring-max 250
 tramp-default-method "ssh"
 vc-follow-symlinks t)

;; Change line truncation indicator
(set-display-table-slot standard-display-table 0 ?↠)
;; Change line continuation indicator
(set-display-table-slot standard-display-table 1 ?←)

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
  "Display quick summary of the current location."
  (interactive)
  (message "%s%s%sline: %s; column: %s"
           (let ((path (or (buffer-file-name) (buffer-name))))
             (format "%s; " (propertize path 'face font-lock-string-face)))
           (if (projectile-project-p)
               (format "project: %s; " (propertize (projectile-project-name) 'face font-lock-type-face)) "") 
           (if-let ((branch (adq/git-current-branch)))
               (format "branch: %s; " (propertize branch 'face font-lock-builtin-face)) "")
           (propertize (format "%d" (line-number-at-pos)) 'face font-lock-constant-face)
           (propertize (format "%d" (current-column)) 'face font-lock-constant-face)))

;; I regularly work on projects with large TAGS files
(advice-add
 'abort-if-file-too-large :around
 (lambda (fn size op-type filename)
   (unless (equal (f-filename filename) "TAGS")
     (apply fn (list size op-type filename)))))

;; Messages buffer bindings
(bind-keys
 :map messages-buffer-mode-map
 ("q" . quit-window)
 ("<escape>" . quit-window)
 ("j" . next-line)
 ("k" . previous-line)
 ("J" . evil-scroll-down)
 ("K" . evil-scroll-up))

(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; FIXME:
;; This could be called before the use-package has installed all the
;; required packages. This can break the first setup.
(defun adq/prog-mode-setup ()
  "Defaults for programming modes."
  (setq-local display-line-numbers t)
  (rainbow-delimiters-mode)
  ;; Fixme this could be done in a neater way
  (unless
      (-contains-p '(web-mode js2-mode) major-mode)
    (rainbow-identifiers-mode))
  (fic-mode)
  (origami-mode)
  (dtrt-indent-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (company-mode)
  (company-statistics-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator " • "))

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

(use-package symbol-overlay
  :ensure t
  :defer t)

(use-package prog-mode
  :defer t
  :init
  ;; Ugly hack to make sure we do not try to use the `adq/prog-mode-setup` hook
  ;; before we have processed all the use-package definitions it depends on.
  (add-hook
   'after-init-hook
   #'(lambda ()
       (add-hook 'prog-mode-hook #'adq/prog-mode-setup))))

(defun adq/text-mode-setup ()
  "Defaults for text modes."
  (gll-guess-language-lite-mode)
  (wc-mode))

(use-package which-func
  :bind (("C-c x w" . which-function-mode))
  :config
  (setq which-func-unknown "-"))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'adq/text-mode-setup))

(use-package recompile-on-save
  :ensure t
  :defer t)

(use-package bury-successful-compilation
  :ensure t
  :init
  (bury-successful-compilation))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t))

(use-package autorevert
  :defer t
  :config
  (setq auto-revert-mode-text " 🅡"))

(use-package sudo-edit
  :ensure t
  :defer t)

(defhydra adq/hydra-buffer nil
  "
^Buffer^              ^Toggle^                   ^Text Size
^^^^^^------------------------------------------------------------
_d_: Diff with file   _w_: Visible whitespace    _-_: Decrease
_r_: Revert buffer    _i_: Indent guides         _+_: Increase
_s_: Change user      _n_: Line numbers
_f_: Where am I?      _t_: Truncate lines
                    ^^_v_: Visual lines
                    ^^_a_: Auto revert
"
  ("d" magit-diff-buffer-file :exit t)
  ("r" revert-buffer :exit t)
  ("s" sudo-edit :exit t)
  ("f" adq/where-am-i :exit t)
  
  ("w" whitespace-mode)
  ("i" highlight-indent-guides-mode)
  ("n" display-line-numbers-mode)
  ("t" toggle-truncate-lines)
  ("v" visual-line-mode)
  ("a" auto-revert-mode)
  
  ("-" text-scale-decrease)
  ("+" text-scale-increase))

(bind-key "C-c e" #'adq/hydra-buffer/body)

(defun adq/find-user-init-file ()
  "Open user's Emacs init file."
  (interactive)
  (find-file user-init-file))

(defun adq/find-file-in-adq ()
  "Open a file belonging to Adequate Emacs.d."
  (interactive)
  (projectile-find-file-in-directory adq/init-directory))

(bind-key "<f9>" #'adq/find-file-in-adq)

(provide 'init-editor)

;;; init-editor.el ends here
