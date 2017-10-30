;;; lang-markdown.el --- Markdown -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for working with Markdown files.

;;; Code:

(fset 'pandoc-pdf-from-buffer
      (adq/make-compiler
       "pandoc"
       (adq/partial concat _ ".pdf")
       (lambda (output) `("-o" ,output "-f" "markdown"))))

;; Patch markdown-mode link jumping to work with links internal to document

(defun adq/markdown-jump-to-top-level-header (title)
  (if-let ((location
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (concat "^#[[:space:]]*" (regexp-quote title) "$") nil t))))
      (goto-char location)
    (error "Target not found")))

(defun adq/markdown-follow-link-at-point-wrap (fn)
  (interactive)
  (if (markdown-link-p)
      (let ((link (markdown-link-url)))
        (if (string-prefix-p "#" link)
            (progn
              (adq/markdown-jump-to-top-level-header (substring link 1))
              (message "%s" link))
          (call-interactively fn)))
    (error "No link at point")))

(defun adq/markdown-setup ()
  (when (locate-library "pandoc-mode")
    (pandoc-mode 1))
  (typo-mode)
  (yas-minor-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'adq/markdown-setup)
  :config
  (setq markdown-bold-underscore t
        markdown-enable-math t
        markdown-command "pandoc")
  (cl-loop for i from 1 to 6
           do (set-face-attribute
               (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
  (set-face-attribute 'markdown-blockquote-face nil :slant 'italic)
  (advice-add 'markdown-follow-link-at-point :around #'adq/markdown-follow-link-at-point-wrap)
  (bind-keys
   :map markdown-mode-map
   ("C-c c c" . pandoc-convert-to-pdf)))

(use-package pandoc-mode
  :if (adq/programs-p "pandoc")
  :ensure t
  :defer t)

(provide 'lang-markdown)

;;; lang-markdown.el ends here
