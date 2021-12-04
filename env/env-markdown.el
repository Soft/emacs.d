;;; env-markdown.el -*- lexical-binding: t; -*-

(use-package pandoc-mode :defer t)

(define-skeleton adq/pandoc-skeleton-yaml-metadata
  "Insert Pandoc YAML metadata block template." nil
  "---"
  > "title: " (skeleton-read "Title: ") \n
  > "author: " (user-full-name) \n
  > "date: " (format-time-string "%A %x") \n
  > "papersize: a4" \n
  > "lang: en" \n
  > "geometry: margin=1in" \n
  > "colorlinks: yes" \n
  > "fontfamily: fbb" \n
  > "fontsize: 12pt" \n
  > "linestretch: 1.25" \n
  > "..." \n \n _)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'adq/markdown-setup)
  (setq markdown-bold-underscore t
        markdown-enable-math t
        markdown-command "pandoc")
  (cl-loop for i from 1 to 6
           do (set-face-attribute
               (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
  (set-face-attribute 'markdown-blockquote-face nil :slant 'italic))

(provide 'env-markdown)
