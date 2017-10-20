;;; lang-org.el --- Org mode -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode configuration.

;; NOTE:
;; Remember to set `org-directory' and `org-default-notes-file' in local configuration.

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
        org-html-doctype "html5"
        org-todo-keywords '((sequence "TODO" "STARTED" "DONE"))
        org-log-done 'time
        org-todo-keyword-faces '(("TODO" . adq/org-todo)
                                 ("STARTED" . adq/org-started)
                                 ("DONE" . adq/org-done)))
  (add-hook 'org-mode-hook #'adq/org-setup))

(use-package org-bullets
  :ensure t
  :defer t)

(bind-keys
 ("C-c o o" . org-capture)
 ("C-c o l" . org-store-link)
 ("C-c o a" . org-agenda)
 ("C-c o b" . org-iswitchb))

(provide 'lang-org)

;;; lang-org.el ends here
