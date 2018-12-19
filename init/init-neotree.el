;;; init-neotree.el --- Neotree setup -*- lexical-binding: t -*-

;;; Commentary:

;; Neotree offers a nice sidebar with directory listing

;;; Code:

(defvar adq/neotree-background-recalibration-percent 5
  "How many percents should neotree background color differ from
  the default background color.")

(defun adq/neotree-setup ()
  "Defaults for neotree buffers."
  (let ((table (make-display-table)))
    (set-display-table-slot table 0 ?\ )
    (setq-local buffer-display-table table))
  (hl-line-mode)
  (face-remap-add-relative
   'default
   :background (adq/color-derive
                adq/neotree-background-recalibration-percent
                (frame-parameter nil 'background-color))))

(defun adq/neotree-open-or-focus ()
  "Open or focus neotree."
  (interactive)
  (if (eq major-mode 'neotree-mode)
      (neotree-hide)
    (cl-loop for window being the windows
             if (eq 'neotree-mode
                    (buffer-local-value 'major-mode (window-buffer window)))
             return (set-frame-selected-window nil window)
             finally (neotree-show))))

(use-package neotree
  :ensure t
  :bind (("<f12>" . adq/neotree-open-or-focus)
         (:map neotree-mode-map
               ("j" . neotree-next-line)
               ("k" . neotree-previous-line)
               ("<escape>" . neotree-hide)))
  :init
  (add-hook 'neotree-mode-hook #'adq/neotree-setup)
  :config
  (setq-default neo-smart-open t
                neo-autorefresh t
                neo-vc-integration '(face char)
                neo-force-change-root t
                neo-show-updir-line nil
                neo-theme 'icons)
  ;; Hide fringes from neotree windows
  (advice-add #'neo-window--init :filter-return
              (lambda (window)
                (set-window-fringes window 0 0)
                window))
  (adq/after-load 'projectile
    (setq projectile-switch-project-action #'neotree-projectile-action)))

(provide 'init-neotree)

;;; init-neotree.el ends here
