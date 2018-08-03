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
        helm-M-x-always-save-history          t
        helm-split-window-in-side-p           t
        helm-ff-skip-boring-files             t
        helm-imenu-fuzzy-match                t
        helm-semantic-fuzzy-match             t
        helm-apropos-fuzzy-match              t
        helm-locate-fuzzy-match               t
        helm-ff-file-name-history-use-recentf t
        helm-lisp-fuzzy-completion            t)
  (add-to-list 'helm-boring-buffer-regexp-list "TAGS")
  (bind-keys
   :map helm-map
   ("<escape>" . helm-keyboard-quit)
   ("C-w" . backward-kill-word))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-c O"   . helm-occur)
   ("C-c S"   . helm-imenu)
   ("M-x"     . helm-M-x)
   ("C-h a"   . helm-apropos)
   ("C-h i"   . helm-info-emacs)
   ("C-c y"   . helm-show-kill-ring)))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on)
  :bind
  (("C-c b" . helm-projectile-find-file)))

(fset 'adq/helm-projectile-search
      (cond ((adq/programs-p "rg") 'helm-projectile-rg)
            ((adq/programs-p "ag") 'helm-projectile-ag)
            ((adq/programs-p "ack") 'helm-projectile-ack)
            ((adq/programs-p "grep") 'helm-projectile-grep)
            (t (lambda ()
                 (interactive)
                 (error "No search program available.")))))

(defun adq/search-helm-or-deadgrep (d)
  "Search project with Helm or, if universal argument is
supplied, with deadgrep."
  (interactive "P")
  (if d
      (deadgrep) ; FIXME: deadgrep might not be available
    (adq/helm-projectile-search)))

(bind-key "C-c s" #'adq/search-helm-or-deadgrep)

(use-package helm-descbinds
  :ensure t
  :bind
  (("C-h b" . helm-descbinds)))

(use-package helm-describe-modes
  :ensure t)

(use-package helm-xref
  :ensure t
  :after (:all xref helm)
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(defun adq/helm-swoop-file-or-project (d)
  "Use Helm Swoop on the current buffer or on the current project
if universal argument is supplied."
  (interactive "P")
  (if d
      (helm-multi-swoop-projectile)
    (helm-swoop)))

(use-package helm-swoop
  :ensure t
  :defer t
  :config
  (setq helm-swoop-speed-or-color       nil
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match      t
        helm-swoop-pre-input-function   (lambda ())))

(bind-key "C-c SPC" #'adq/helm-swoop-file-or-project)

(use-package helm-themes
  :ensure t
  :bind
  (("C-x c T" . helm-themes)))

(use-package helm-emoji
  :bind (("C-x c E" . helm-emoji)))

(use-package helm-make
  :ensure t
  :defer t
  :bind (("C-c C-p M" . helm-make-projectile)))

;; This is also bound into the bookmark hydra in init-bookmarks.el
(use-package helm-bm
  :ensure t
  :bind  (("C-x c M" . helm-bm)))

(provide 'init-helm)

;;; init-helm.el ends here
