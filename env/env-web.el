;;; init-web.el -*- lexical-binding: t; -*-

(use-package css-eldoc
  :hook ((css-mode scss-mode) . css-eldoc-enable))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)))

(use-package css-mode
  :straight nil
  :mode (("\\.css\\'" . css-mode)))

(use-package scss-mode
  :mode (("\\.scss\''" . scss-mode)))

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

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (add-hook 'js2-mode-hook #'adq/js-setup))

(use-package web-mode
  :mode (("\\.p?html?\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.j2\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode))
  :config
  (setq
   web-mode-enable-css-colorization t
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-block-padding 0
   web-mode-style-padding 0
   web-mode-script-padding 0))

(provide 'env-web)
