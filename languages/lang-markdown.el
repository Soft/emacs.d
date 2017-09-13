;;; lang-markdown.el --- Markdown -*- lexical-binding: t -*-

(fset 'pandoc-pdf-from-buffer
      (make-compiler
       "pandoc"
       (partial concat _ ".pdf")
       (lambda (output) `("-o" ,output "-f" "markdown"))))

;; Patch markdown-mode link jumping to work with links internal to document

(defun markdown-jump-to-top-level-header (title)
  (if-let ((location
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (concat "^#[[:space:]]*" title "$")))))
      (goto-char location)
    (error "Target not found")))

(defun markdown-follow-link-at-point-wrap (fn)
  (interactive)
  (if (markdown-link-p)
      (let ((link (markdown-link-link)))
        (if (string-prefix-p "#" link)
            (progn
              (markdown-jump-to-top-level-header (substring link 1))
              (message "%s" link))
          (call-interactively fn)))
    (error "No link at point")))

(defun markdown-setup ()
  (when (locate-library "pandoc-mode")
    (pandoc-mode 1)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'markdown-setup)
  :config
  (setq markdown-bold-underscore t
        markdown-enable-math t
        markdown-command "pandoc")
  (cl-loop for i from 1 to 6
           do (set-face-attribute
               (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
  (set-face-attribute 'markdown-blockquote-face nil :slant 'italic)
  (advice-add 'markdown-follow-link-at-point :around #'markdown-follow-link-at-point-wrap))

(use-package pandoc-mode
  :if (programs-p "pandoc")
  :ensure t
  :defer t)

(provide 'lang-markdown)
