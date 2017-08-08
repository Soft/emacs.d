
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
    (progn
      (setq markdown-bold-underscore t
            markdown-enable-math t
            markdown-command "pandoc")
      (cl-loop for i from 1 to 6
           do (set-face-attribute
               (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
      (set-face-attribute 'markdown-blockquote-face nil :slant 'italic)))

(provide 'lang-markdown)