;;; init-sidebar.el --- Sidebar setup -*- lexical-binding: t -*-

;;; Commentary:

;; Treemacs sidebar with directory listing

;;; Code:

(defvar adq/treemacs-background-recalibration-percent 5
  "How many percents should treemacs background color differ from
  the default background color.")

(defun adq/treemacs-setup ()
  "Defaults for treemacs buffers."
  (face-remap-add-relative
   'default
   :background
   (adq/color-derive
    adq/treemacs-background-recalibration-percent
    (frame-parameter nil 'background-color))))


(use-package treemacs
  :ensure t
  :bind (("<f12>" . treemacs))
  :init
  (add-hook 'treemacs-mode-hook #'adq/treemacs-setup)
  :config
  (setq treemacs-indentation 1))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(provide 'init-sidebar)

;;; init-sidebar.el ends here
