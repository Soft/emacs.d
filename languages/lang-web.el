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

(use-package web-beautify
  :ensure t
  :defer t)

(defvar adq/js-prettify-symbols-alist
  '(("==" . ?≈)
    ("===" . ?≡)
    ("!==" . ?≠)
    ("!=" . ?≉)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("=>" . ?⇒)
    ("..." . ?…))
  "Symbol prettification alist for `js2-mode'.")

(defun adq/js-setup ()
  "Defaults for JavaScript."
  (setq-local prettify-symbols-alist adq/js-prettify-symbols-alist))

(defun adq/web-setup ()
  "Defaults for `web-mode'."
  (adq/setq-local
      web-mode-enable-css-colorization t
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-block-padding 0
      web-mode-style-padding 0
      web-mode-script-padding 0))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'adq/js-setup)
  :bind
  (:map js2-mode-map
        ("C-c a =" . web-beautify-js)))

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode))
  :ensure t
  :init
  (add-hook 'rjsx-mode-hook #'adq/js-setup)
  :bind
  (:map rjsx-mode-map
        ("C-c a =" . web-beautify-js)))

(use-package js-comint
  :if (adq/programs-p "node")
  :ensure t
  :defer t)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :ensure t)

(defun adq/css-setup ()
  "Defaults for CSS."
  (rainbow-mode)
  (aggressive-indent-mode)
  (css-eldoc-enable))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :init
  (add-hook 'css-mode-hook #'adq/css-setup)
  :bind
  (:map css-mode-map
        ("C-c a =" . web-beautify-css)))

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
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.j2\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode))
  :ensure t
  :init
  (add-hook 'web-mode-hook #'adq/web-setup)
  :bind
  (:map web-mode-map
        ("C-c a =" . web-beautify-html)))

(provide 'lang-web)

;;; lang-web.el ends here
