
(use-package css-mode
  :ensure t
  :config
  (use-package css-eldoc
    :ensure t))

;; This isn't really the right place for this
(use-package rainbow-mode
  :ensure t
  :init
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

(provide 'lang-css)
