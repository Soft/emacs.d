;;; env-lisp.el -*- lexical-binding: t; -*-

(use-package elisp-refs :defer t)

(use-package package-lint :defer t)

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
  :init
  (add-hook 'emacs-lisp-mode-hook #'adq/lisp-setup))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(provide 'env-lisp)
