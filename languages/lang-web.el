;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Configuration for various web related languages

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :ensure t)

(use-package css-eldoc
  :defer t
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package zenity-color-picker
  :if (programs-p "zenity")
  :ensure t
  :defer t)

(defun css-setup ()
  (rainbow-mode)
  (css-eldoc-enable))

(use-package css-mode
  :ensure t
  :mode (("\\.css\\'" . css-mode))
  :init
  (add-hook 'css-mode-hook #'css-setup))

(use-package scss-mode
  :mode (("\\.scss\''" . scss-mode))
  :ensure t
  :init
  (add-hook 'css-mode-hook #'css-setup))

(provide 'lang-web)
