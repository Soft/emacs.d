
(defun find-user-init-file ()
  (interactive)
  (find-file user-init-file))

(bind-key "<f4>" 'find-user-init-file)

(use-package macrostep
  :ensure t)

(use-package hl-sexp
  :ensure t)

(use-package paredit
  :diminish paredit-mode
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(defun lisp-setup ()
  "Defaults for lisp-like modes"
  (hl-sexp-mode)
  (aggressive-indent-mode)
  (enable-paredit-mode)
  (evil-paredit-mode)
  (setq indent-tabs-mode nil
	tab-width 2))

(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'lisp-setup))

(provide 'lang-lisp)
