;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Configuration for various web related languages

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :ensure t)

(use-package css-mode
  :ensure t
  :mode (("\\.css\\'" . css-mode))
  :config
  (use-package css-eldoc
    :ensure t))

(use-package scss-mode
  :mode (("\\.scss\''" . scss-mode))
  :ensure t)

;; This isn't really the right place for this
;; Does this even work since those modes are lazy-loaded
(use-package rainbow-mode
  :ensure t
  :init
  (dolist (hook '(css-mode-hook html-mode-hook scss-mode-hook))
    (add-hook hook 'rainbow-mode)))

(use-package zenity-color-picker
  :if (programs-p "zenity")
  :ensure t
  :defer t)

(provide 'lang-web)
