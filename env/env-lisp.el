;;; env-lisp.el -*- lexical-binding: t; -*-

(use-package elisp-refs
  :bind (:map emacs-lisp-mode-map
              ("C-c a s f" . elisp-refs-function)
              ("C-c a s m" . elisp-refs-macro)
              ("C-c a s v" . elisp-refs-variable)
              ("C-c a s s" . elisp-refs-symbol)
              ("C-c a s S" . elisp-refs-special)))

(use-package package-lint :defer t)

(use-package macrostep :defer t)

(use-package highlight-quoted :defer t)

(use-package highlight-blocks
  :defer t
  :config
  (setq highlight-blocks-max-innermost-block-count 1))

(use-package paredit
  :defer t
  :diminish paredit-mode)

(use-package evil-paredit
  :hook (paredit-mode . evil-paredit-mode))

(defun adq/eval-and-replace-region (from to)
  "Replace region with its evaluated form."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (eval (read (delete-and-extract-region from to)))
          (current-buffer)))))

(defvar adq/emacs-lisp-scratch-name "*elisp scratch*"
  "Emacs lisp scratch buffer name.")

(defun adq/emacs-lisp-scratch (d)
  "Switch to scratch buffer for evaluating Emacs lisp. If the
buffer does not exist it is created. If universal argument is
supplied, new buffer is always created."
  (interactive "P")
  (with-current-buffer
      (if d (generate-new-buffer adq/emacs-lisp-scratch-name)
        (get-buffer-create adq/emacs-lisp-scratch-name))
    (unless (eq major-mode 'lisp-interaction-mode)
      (lisp-interaction-mode))
    (pop-to-buffer (current-buffer))))

(use-package ielm
  :bind
  (("C-c i" . ielm))
  :init
  (setq ielm-prompt "λ> "
        ielm-noisy nil))

(defun adq/lisp-setup ()
  "Defaults for lisp-like modes"
  (paredit-mode)
  (highlight-blocks-mode)
  (highlight-quoted-mode)
  (aggressive-indent-mode))

(use-package elisp-mode
  :straight nil
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c a r" . eval-region)
              ("C-c a b" . eval-buffer)
              ("C-c a f" . eval-defun))
  :init
  (add-hook 'emacs-lisp-mode-hook #'adq/lisp-setup))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook #'adq/lisp-setup)
  (add-hook 'clojurescript-mode-hook #'adq/lisp-setup))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :bind (:map clojure-mode-map
              ("C-c a j" . cider-jack-in)
              ("C-c a r" . cider-eval-region)
              ("C-c a b" . cider-eval-buffer)
              ("C-c a f" . cider-eval-defun-at-point))
  :config
  (setq cider-repl-display-help-banner nil
        cider-show-error-buffer 'except-in-repl))

(provide 'env-lisp)
