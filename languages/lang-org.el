;;; lang-org.el --- Org mode -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode configuration.

;; NOTE: Remember to set `org-directory', `org-default-notes-file' and
;; `org-agenda-files' in local configuration.

;;; Code:

(defun adq/org-setup ()
  "Setup org-mode"
  (org-bullets-mode)
  (yas-minor-mode))

(defface adq/org-todo
  '((t :foreground "#E53935"))
  "Face for org-mode TODO items.")

(defface adq/org-started
  '((t :foreground "#FDD835"))
  "Face for org-mode STARTED items.")

(defface adq/org-done
  '((t :foreground "#8BC34A"))
  "Face for org-mode DONE items.")

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :defer t
  :init
  (setq org-M-RET-may-split-line nil
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-html-doctype "html5"
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "DONE(d)"))
        org-log-done 'time
        org-todo-keyword-faces '(("TODO" . adq/org-todo)
                                 ("STARTED" . adq/org-started)
                                 ("DONE" . adq/org-done)))
  (add-hook 'org-mode-hook #'adq/org-setup)
  :config
  (use-package evil-org
    :ensure t
    :after evil
    :config
    (add-hook 'org-mode-hook #'evil-org-mode)
    (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)))

(use-package org-bullets
  :ensure t
  :defer t
  :config
  (setq org-bullets-bullet-list
        '("◉" "○" "⯈" "🟊" "✪" "❧" "❥" "♫" "🟋")))

(use-package helm-org-files
  :if (featurep 'helm)
  :bind (("C-c o f" . helm-org-files)))

(bind-keys
 ("C-c o o" . org-capture)
 ("C-c o l" . org-store-link)
 ("C-c o a" . org-agenda)
 ("C-c o b" . org-iswitchb))

(provide 'lang-org)

;;; lang-org.el ends here
