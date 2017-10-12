;;; init-completion.el --- Code completion -*- lexical-binding: t -*-

;;; Commentary:

;; Use Company for code completion.

;;; Code:

(use-package company-quickhelp
  :ensure t
  :defer t
  :config
  (add-hook
   'adq/switch-theme-hook
   (lambda ()
     ;; After theme change, derive popup colors from theme.
     (setq company-quickhelp-color-background
           (adq/color-derive 5 (frame-parameter nil 'background-color))
           company-quickhelp-color-foreground
           (frame-parameter nil 'foreground-color)))))

(use-package company-statistics
  :ensure t
  :defer t)

(use-package helm-company
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook
   'after-init-hook
   (lambda ()
     (global-company-mode)
     (company-statistics-mode)))
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-echo-delay 0
        company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  ;; There is something wrong wit this where the popup seems to cause
  ;; company-active-map disablind
  ;; (add-hook 'company-mode-hook #'company-quickhelp-mode)
  (bind-keys
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-j" . company-complete-selection)
   ("C-/" . helm-company))
  (adq/after-load 'evil
    (evil-declare-change-repeat 'company-complete)
    (bind-key "C-n" #'company-complete evil-insert-state-map)))

(provide 'init-completion)

;;; init-completion.el ends here
