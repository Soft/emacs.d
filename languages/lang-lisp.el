;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Lisp-like languages

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

(use-package macrostep
  :defer t
  :ensure t
  :diminish macrostep-mode)

(use-package elisp-refs
  :defer t
  :ensure t)

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

(use-package geiser
  :if (programs-p "guile" "racket")
  :ensure t
  :defer t)

(use-package slime
  :if (programs-p "sbcl")
  :ensure t
  :defer t)

(defun lisp-setup ()
  "Defaults for lisp-like modes"
  (hl-sexp-mode)
  (aggressive-indent-mode)
  (enable-paredit-mode)
  (evil-paredit-mode)
  (setq indent-tabs-mode nil
        tab-width 2))

;; FIXME: Make macrostep play nice with Evil
(use-package elisp-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lisp-setup)
  :config
  (bind-keys
   :map emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

(provide 'lang-lisp)
