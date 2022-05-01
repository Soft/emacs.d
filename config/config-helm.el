;;; config-helm.el -*- lexical-binding: t; -*-

(use-package helm
  :hook (after-init . helm-mode)
  :diminish helm-mode
  :init
  (require 'helm-config)
  :bind
  (("<remap> <find-file>" . helm-find-files)
   ("<remap> <list-buffers>" . helm-buffers-list)
   ("<remap> <switch-to-buffer>" . helm-mini)
   ("<remap> <execute-extended-command>" . helm-M-x)
   ("<remap> <apropos-command>" . helm-apropos)
   ("<remap> <info>" . helm-info-emacs)
   ("<remap> <list-bookmarks>" . helm-bookmarks)
   ("C-c k" . helm-imenu)
   ("C-c y" . helm-show-kill-ring)
   :map helm-map
   ("<escape>" . helm-keyboard-quit)
   ("C-w" . backward-kill-word))
  :config
  (setq
   helm-display-header-line nil
   helm-ff-file-name-history-use-recentf t
   helm-M-x-always-save-history t
   helm-split-window-in-side-p t
   helm-ff-skip-boring-files t)
  (setq
   helm-M-x-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-file-cache-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-locate-fuzzy-match t
   helm-recentf-fuzzy-match t
   helm-semantic-fuzzy-match t)
  (add-to-list 'helm-boring-buffer-regexp-list "compile_commands.json")
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'helm)))

(use-package helm-posframe
  :disabled t
  :after helm
  :config
  (defun adq/helm-posframe-get-size ()
    (let ((width (floor (* (frame-parameter nil 'width) 0.5)))
          (height (floor (* (frame-parameter nil 'height) 0.6))))
      (list
       :width width
       :height height
       :min-width width
       :min-height height)))
  (setq
   helm-echo-input-in-header-line t
   helm-posframe-poshandler #'posframe-poshandler-frame-top-center
   helm-posframe-size-function #'adq/helm-posframe-get-size)
  (helm-posframe-enable))

(use-package helm-themes
  :after helm)

(use-package helm-projectile
  :after (helm projectile)
  :hook (projectile-mode . helm-projectile-on)
  :bind (("C-c b" . helm-projectile-find-file)))

(use-package helm-swoop
  :after helm
  :bind (("C-c SPC" . helm-swoop))
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-line-number-face t))

(adq/use-local-package helm-fd :defer t)

(use-package helm-descbinds :defer t)

(use-package helm-describe-modes :defer t)

(defun adq/helm-projectile-rg ()
  "Search in project or default directory using ripgrep."
  (interactive)
  (let ((default-directory
          (or (projectile-project-root)
              (when-let (file (buffer-file-name))
                (file-name-directory file))
              default-directory))
        (helm-grep-ag-command
         "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --no-heading -F --line-number %s %s %s"))
    (call-interactively #'helm-do-grep-ag)))

(bind-key "C-c s" #'adq/helm-projectile-rg)

(provide 'config-helm)
