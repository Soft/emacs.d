;;; lang-web.el --- Web-related languages -*- lexical-binding: t -*-

;;; Commentary:

;; Support for various Web programming languages.

;;; Code:

(use-package css-eldoc
  :defer t
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(defun adq/js-setup ()
  (rainbow-identifiers-mode -1))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'adq/js-setup))

(use-package js-comint
  :if (adq/programs-p "node")
  :ensure t
  :defer t)

(use-package web-beautify
  :if (adq/programs-p "js-beautify")
  :ensure t
  :defer t)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :ensure t)

(defun adq/css-setup ()
  (rainbow-mode)
  (css-eldoc-enable))

(use-package css-mode
  :ensure t
  :mode (("\\.css\\'" . css-mode))
  :init
  (add-hook 'css-mode-hook #'adq/css-setup))

(use-package scss-mode
  :mode (("\\.scss\''" . scss-mode))
  :ensure t
  :init
  (add-hook 'css-mode-hook #'adq/css-setup))

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

;;; lang-web.el ends here
