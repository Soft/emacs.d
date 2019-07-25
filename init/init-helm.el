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
  (adq/add-to-list-many 'helm-boring-buffer-regexp-list
                        '("TAGS" "compilation_commands.json"))
  (defhydra adq/helm-hydra nil
    "Helm"
    ("j" helm-next-line)
    ("k" helm-previous-line)
    ("J" helm-next-page)
    ("K" helm-previous-page)
    ("l" helm-next-source)
    ("h" helm-previous-source)
    ("f" helm-toggle-full-frame)
    ("m" helm-toggle-visible-mark)
    ("M" helm-mark-all)
    ("g" helm-beginning-of-buffer)
    ("G" helm-end-of-buffer)
    ("<tab>" helm-select-action)
    ("RET" helm-execute-persistent-action)
    ("1" (helm-select-nth-action 0))
    ("2" (helm-select-nth-action 1))
    ("3" (helm-select-nth-action 2))
    ("4" (helm-select-nth-action 3))
    ("5" (helm-select-nth-action 4))
    ("6" (helm-select-nth-action 5))
    ("7" (helm-select-nth-action 6))
    ("8" (helm-select-nth-action 7))
    ("9" (helm-select-nth-action 8))
    ("0" (helm-select-nth-action 9))
    ("<escape>" nil))
  (bind-keys
   :map helm-map
   ("<escape>" . helm-keyboard-quit)
   ("C-w" . backward-kill-word)
   ("M-SPC" . adq/helm-hydra/body))
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
  (setq helm-swoop-speed-or-color       t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match      nil
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

(use-package helm-fd
  :commands (helm-fd)
  :bind (("C-x c d" . helm-fd)))

(use-package helm-snippets
  :functions (helm-snippets-define)
  :commands (helm-snippets-arrow helm-snippets-star)
  :bind (("C-x 8 A" . helm-snippets-arrow)
         ("C-x 8 s" . helm-snippets-star)))

(use-package helm-systemd
  :ensure t
  :if (adq/programs-p "systemctl")
  :bind (("C-x c S" . helm-systemd)))

(provide 'init-helm)

;;; init-helm.el ends here
