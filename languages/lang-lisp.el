;;; lang-lisp.el --- Lisp-like languages -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for different Lisp varieties.

;;; Code:

(defun adq/ielm-setup ()
  (highlight-blocks-mode)
  (rainbow-delimiters-mode))

(use-package ielm 
  :defer t
  :init
  (setq ielm-prompt "λ> "
        ielm-noisy nil)
  :config
  (add-hook 'ielm-mode-hook #'adq/ielm-setup))

(use-package macrostep
  :defer t
  :ensure t
  :diminish macrostep-mode
  :config
  (adq/after-load 'evil
    (evil-make-overriding-map macrostep-keymap 'normal)
    (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)))

(use-package elisp-refs
  :ensure t
  :defer t)

(use-package highlight-blocks
  :defer t
  :ensure t
  :config
  (setq highlight-blocks-max-innermost-block-count 1))

(use-package highlight-quoted
  :defer t
  :ensure t)

(use-package highlight-numbers
  :defer t
  :ensure t)

(use-package paredit
  :defer t
  :diminish paredit-mode
  :ensure t)

(use-package evil-paredit
  :defer t
  :ensure t)

(use-package aggressive-indent
  :defer t
  :ensure t
  :diminish aggressive-indent-mode)

(defun adq/lisp-setup ()
  "Defaults for lisp-like modes"
  (highlight-blocks-mode)
  (highlight-quoted-mode)
  (highlight-numbers-mode)
  (aggressive-indent-mode)
  (enable-paredit-mode)
  (evil-paredit-mode)
  (setq indent-tabs-mode nil
        tab-width 2))

(defun adq/eval-and-replace-region (from to)
  "Replace region with its evaluated form."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (eval (read (delete-and-extract-region from to)))
          (current-buffer)))))

(defun adq/sort-symbol-list-region (from to)
  "Sort list in region."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (-sort #'string< (read (delete-and-extract-region from to)))
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

(defun adq/ielm-or-scratch (d)
  "Switch to IELM or Emacs lisp scratch buffer if universal
argument is supplied."
  (interactive "P")
  (if d (adq/emacs-lisp-scratch nil)
    (ielm)))

(bind-key "C-c i" #'adq/ielm-or-scratch)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-format
  :ensure t
  :defer t)

(adq/region-switch-command adq/elisp-format-region-or-buffer
  #'elisp-format-region #'elisp-format-buffer
  "Use elisp-format to format region or buffer.")

(use-package elisp-mode
  :defer t
  :bind
  (:map
   emacs-lisp-mode-map
   ("C-c a =" . adq/elisp-format-region-or-buffer)

   ("C-c a m" . macrostep-expand)

   ("C-c a r" . eval-region)
   ("C-c a b" . eval-buffer)
   ("C-c a f" . eval-defun)
   ("C-c a e" . adq/eval-and-replace-region)

   ("C-c a s f" . elisp-refs-function)
   ("C-c a s m" . elisp-refs-macro)
   ("C-c a s v" . elisp-refs-variable)
   ("C-c a s s" . elisp-refs-symbol)
   ("C-c a s S" . elisp-refs-special)
   :map
   lisp-interaction-mode-map
   ("C-<return>" . eval-print-last-sexp)
   ("C-c a =" . adq/elisp-format-region-or-buffer)

   ("C-c a m" . macrostep-expand)

   ("C-c a r" . eval-region)
   ("C-c a b" . eval-buffer)
   ("C-c a f" . eval-defun)
   ("C-c a e" . adq/eval-and-replace-region)

   ("C-c a s f" . elisp-refs-function)
   ("C-c a s m" . elisp-refs-macro)
   ("C-c a s v" . elisp-refs-variable)
   ("C-c a s s" . elisp-refs-symbol)
   ("C-c a s S" . elisp-refs-special))
  :init
  (add-hook 'emacs-lisp-mode-hook #'adq/lisp-setup))

(use-package geiser
  :if (adq/programs-p "guile" "racket")
  :ensure t
  :defer t)

(use-package slime
  :if (adq/programs-p "sbcl")
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook #'adq/lisp-setup))

(provide 'lang-lisp)

;;; lang-lisp.el ends here
