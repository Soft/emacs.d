;;; init-helm.el --- Helm configuration -*- lexical-binding: t -*-

;;; Commentary:

;; I really like the smooth search-oriented approach to interfaces that Helm
;; provides.

;;; Code:

(use-package helm 
  :ensure t
  :diminish helm-mode
  :init
  (require 'dired) ;; This seems to fix a strange problem with dired-buffers
  (require 'helm-config)
  (helm-mode)
  :config
  (setq helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-M-x-fuzzy-match                  t
        helm-split-window-in-side-p           t
        helm-ff-skip-boring-files             t
        helm-imenu-fuzzy-match                t
        helm-semantic-fuzzy-match             t
        helm-apropos-fuzzy-match              t
        helm-locate-fuzzy-match               t
        helm-ff-file-name-history-use-recentf t
        helm-lisp-fuzzy-completion            t)
  ;; (if (programs-p "curl")
  ;;     (setq helm-net-prefer-curl t))
  (bind-keys
   :map helm-map
   ("<escape>" . helm-keyboard-quit)
   ("C-w" . backward-kill-word))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-c o"   . helm-occur)
   ("C-c S"   . helm-imenu)
   ("M-x"     . helm-M-x)
   ("C-h a"   . helm-apropos)
   ("C-h i"   . helm-info-emacs)))

(use-package helm-ag
  :if (programs-p "ag")
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on)
  :bind
  (("C-c b" . helm-projectile-find-file)
   ("C-c s" . helm-projectile-ag)))

(use-package helm-descbinds
  :ensure t
  :bind
  (("C-h b" . helm-descbinds)))

(defun helm-swoop-projectile-or-file ()
  (interactive)
  (if (projectile-project-p)
      (helm-multi-swoop-projectile)
    (helm-swoop)))

(use-package helm-swoop
  :ensure t
  :defer t
  :config
  (setq helm-swoop-speed-or-color       t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match      t))

(bind-key "C-c SPC" #'helm-swoop-projectile-or-file)

(use-package helm-themes
  :ensure t
  :bind
  (("C-x c T" . helm-themes)))

(use-package helm-emoji
  :bind (("C-x c E" . helm-emoji)))

(use-package helm-make
  :ensure t
  :defer t
  :bind (("C-c p M" . helm-make-projectile)))

(use-package helm-bm
  :ensure t
  :bind  (("C-x c M" . helm-bm)))

(provide 'init-helm)

;;; init-helm.el ends here
