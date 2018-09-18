;;; lang-lisp.el --- Lisp-like languages -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for different Lisp varieties.

;;; Code:

(defun adq/ielm-setup ()
  (highlight-blocks-mode)
  (rainbow-delimiters-mode))

(use-package ielm 
  :bind (("C-c i" . ielm))
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
  (:map emacs-lisp-mode-map
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
