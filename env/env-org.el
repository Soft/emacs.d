;;; env-markdown.el -*- lexical-binding: t; -*-

(defface adq/org-todo
  '((t :foreground "#E53935"))
  "Face for org-mode TODO items.")

(defface adq/org-started
  '((t :foreground "#FDD835"))
  "Face for org-mode STARTED items.")

(defface adq/org-done
  '((t :foreground "#8BC34A"))
  "Face for org-mode DONE items.")

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉" "○" "⯈" "🟊" "✪" "❧" "❥" "♫" "🟋")))

(use-package org
  :mode (("\\.org\\'" . org-mode))
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
                                 ("DONE" . adq/org-done))))

(provide 'env-org)
