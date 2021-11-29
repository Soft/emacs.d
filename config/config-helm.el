;;; config-helm.el -*- lexical-binding: t; -*-

(use-package helm
  :hook (after-init . helm-mode)
  :diminish helm-mode
  :init
  (require 'helm-config)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-c O" . helm-occur)
   ("C-c k" . helm-imenu)
   ("M-x" . helm-M-x)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs)
   ("C-c y" . helm-show-kill-ring)
   :map helm-map
   ("<escape>" . helm-keyboard-quit)
   ("C-w" . backward-kill-word))
  :config
  (setq
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-M-x-always-save-history t
   helm-split-window-in-side-p t
   helm-ff-skip-boring-files t
   helm-imenu-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-locate-fuzzy-match t
   helm-ff-file-name-history-use-recentf t
   helm-lisp-fuzzy-completion t)
  (add-to-list 'helm-boring-buffer-regexp-list "compile_commands.json")
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'helm)))

(defun adq/helm-posframe-get-size ()
  (let ((width (floor (* (frame-parameter nil 'width) 0.5)))
        (height (floor (* (frame-parameter nil 'height) 0.6))))
    (list
     :width width
     :height height 
     :min-width width
     :min-height height)))

(use-package helm-posframe
  :after helm
  :config
  (setq helm-posframe-poshandler #'posframe-poshandler-frame-top-center
        helm-posframe-size-function #'adq/helm-posframe-get-size)
  (helm-posframe-enable))

(use-package helm-projectile
  :after (helm projectile)
  :hook (projectile-mode . helm-projectile-on)
  :bind (("C-c b" . helm-projectile-find-file)))

(use-package helm-swoop
  :bind (("C-c SPC" . helm-swoop))
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-line-number-face t))

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
