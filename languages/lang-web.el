;;; lang-web.el --- Web related languages -*- lexical-binding: t -*-

(use-package css-eldoc
  :defer t
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :ensure t)

(use-package js-comint
  :if (programs-p "node")
  :ensure t
  :defer t)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :ensure t)

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

(use-package haml-mode
  :mode (("\\.haml\\'" . haml-mode))
  :ensure t)

(use-package web-mode
  :mode (("\\.p?html?\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :ensure t)

(provide 'lang-web)
