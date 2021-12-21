;;; config-vertico.el -*- lexical-binding: t; -*-

(use-package vertico
  :hook (after-init . vertico-mode)
  :diminish vertico-mode
  :config
  (setq vertico-count 25
        vertico-cycle t))

(use-package all-the-icons-completion
  :defer t)

(use-package marginalia
  :after vertico
  :diminish marginalia-mode
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode)
  (marginalia-mode))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (("<remap> <switch-to-buffer>" . consult-buffer)
   ("<remap> <switch-to-buffer-other-window>" . consult-buffer-other-window)
   ("<remap> <switch-to-buffer-other-frame>" . consult-buffer-other-frame)
   ("<remap> <apropos-command>" . consult-apropos)
   ("<remap> <repeat-complex-command>" . consult-complex-command)
   ("<remap> <load-theme>" . consult-theme)
   ("C-c k" . consult-imenu)
   ("C-c y" . consult-yank-from-kill-ring)
   ("C-c SPC" . consult-line)))

(use-package consult-projectile
  :after projectile
  :bind
  (("C-c b" . consult-projectile)))

(defun adq/consult-projectile-rg ()
  "Search in project or default directory using ripgrep."
  (interactive)
  (let ((default-directory
          (or (projectile-project-root)
              (when-let (file (buffer-file-name))
                (file-name-directory file))
              default-directory)))
    (call-interactively #'consult-ripgrep)))

(bind-key "C-c s" #'adq/consult-projectile-rg)

(provide 'config-vertico)
