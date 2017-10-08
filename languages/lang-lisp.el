;;; lang-lisp.el --- Lisp-like languages -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for different Lisp varieties.

;;; Code:

(defun find-user-init-file ()
  "Open user's emacs init file."
  (interactive)
  (find-file user-init-file))

(bind-key "<f4>" #'find-user-init-file)

(defun ielm-setup ()
  (hl-sexp-mode)
  (rainbow-delimiters-mode))

(use-package ielm 
  :bind (("C-c i" . ielm))
  :init
  (setq ielm-prompt "λ> ")
  :config
  (add-hook 'ielm-mode-hook #'ielm-setup))

;; FIXME: Make macrostep play nice with Evil
(use-package macrostep
  :defer t
  :ensure t
  :diminish macrostep-mode)

(use-package elisp-refs
  :ensure t
  :defer t)

(use-package hl-sexp
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

(defun lisp-setup ()
  "Defaults for lisp-like modes"
  (hl-sexp-mode)
  (aggressive-indent-mode)
  (enable-paredit-mode)
  (evil-paredit-mode)
  (setq indent-tabs-mode nil
        tab-width 2))

(defun eval-and-replace-region (from to)
  "Replace region with its evaluated form."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (eval (read (delete-and-extract-region from to)))
          (current-buffer)))))

(defun sort-symbol-list-region (from to)
  "Sort list in region."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (-sort #'string< (read (delete-and-extract-region from to)))
          (current-buffer)))))

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lisp-setup)
  :config
  (bind-keys
   :map emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)

   ("C-c a r" . eval-region)
   ("C-c a b" . eval-buffer)
   ("C-c a f" . eval-defun)
   ("C-c a e" . eval-and-replace-region)

   ("C-c r f" . elisp-refs-function)
   ("C-c r m" . elisp-refs-macro)
   ("C-c r v" . elisp-refs-variable)
   ("C-c r s" . elisp-refs-symbol)
   ("C-c r S" . elisp-refs-special)))

(use-package geiser
  :if (programs-p "guile" "racket")
  :ensure t
  :defer t)

(use-package slime
  :if (programs-p "sbcl")
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook #'lisp-setup))

(provide 'lang-lisp)

;;; lang-lisp.el ends here
