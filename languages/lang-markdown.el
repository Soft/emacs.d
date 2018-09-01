;;; lang-markdown.el --- Markdown -*- lexical-binding: t -*-

;;; Commentary:

;; Tools for working with Markdown files.

;;; Code:

(defvar-local adq/pandoc-pdf-from-buffer-use-citeproc nil
  "Use pandoc-citeproc with `adq/pandoc-pdf-from-buffer'.")

(defvar-local adq/pandoc-pdf-target-file nil
  "Path to the pandoc target.")

(defun adq/pandoc-toggle-citeproc ()
  "Toggle use of pandoc-citeproc with
  `adq/pandoc-pdf-from-buffer'."
  (interactive)
  (message
   (if (setq-local adq/pandoc-pdf-from-buffer-use-citeproc
                   (not adq/pandoc-pdf-from-buffer-use-citeproc))
       "pandoc-citeproc enabled"
     "pandoc-citeproc disabled")))

(defun adq/pandoc-bibliography-present-p ()
  "Check if Pandoc yaml metadata block contains bibliography
  key."
  (save-excursion
    (goto-char (point-min))
    (when-let ((start (re-search-forward (rx line-start "---" line-end) nil t))
               (end (re-search-forward (rx line-start "..." line-end) nil t)))
      (goto-char start)
      (and (re-search-forward (rx line-start "bibliography: ") end t) t))))

(defun adq/pandoc-pdf-open-target ()
  "Open target document created with
`adq/pandoc-pdf-from-buffer'."
  (interactive)
  (if adq/pandoc-pdf-target-file
      (call-process adq/opener nil 0 nil adq/pandoc-pdf-target-file)
    (error "No target file. Make sure to run `adq/pandoc-pdf-from-buffer' first.")))

(adq/compiler-command adq/pandoc-pdf-from-buffer
  "Compile markdown to PDF using Pandoc."
  "pandoc"
  (let ((out (concat it ".pdf")))
    (setq-local adq/pandoc-pdf-target-file out)
    `(,out . (,@(if adq/pandoc-pdf-from-buffer-use-citeproc
                    '("--filter" "pandoc-citeproc") '())
              "-o" ,out "-f" "markdown"))))

;; Patch markdown-mode link jumping to work with links internal to document

(defun adq/markdown-derive-auto-identifier (title)
  "Derive identifier based on title. The steps to do this are
  outlined in the Pandoc manual. This is not a complete
  implementation as it does not remove all formating from the
  title as this would require a proper parser."
  (let ((identifier
         (thread-last title
           (downcase)
           (replace-regexp-in-string " " "-")
           (replace-regexp-in-string (rx line-start (0+ (not alpha))) ""))))
    (if (string= "" identifier)
        "section"
      identifier)))

(defun adq/markdown-list-titles ()
  "Return list of (SECTION-TITLE . POSITION) pairs."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (re-search-forward
                      (rx line-start
                          (repeat 1 6 "#")
                          (0+ whitespace)
                          (group (1+ not-newline)))
                      nil t)
               collect (cons (match-string-no-properties 1) (match-beginning 1))))))

(defun adq/markdown-title-identifiers ()
  "Return hash table with document identifers as keys."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (title (adq/markdown-list-titles))
      (puthash (adq/markdown-derive-auto-identifier (car title)) t hash))
    hash))

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

;; Flycheck checker for invalid internal links

(defun adq/markdown-check-internal-links (checker callback)
  "Checker function for Flycheck. The checker checks the buffer
  for internal links that do not have a target."
  (let ((identifiers (adq/markdown-title-identifiers))
        (errors '()))
    (dolist (link-type (list markdown-regex-link-inline
                             markdown-regex-link-reference
                             markdown-regex-uri
                             markdown-regex-angle-uri))
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (cl-loop while (re-search-forward link-type nil t)
                   for url = (markdown-link-url)
                   when (string-prefix-p "#" url)
                   unless (gethash (adq/markdown-derive-auto-identifier (substring url 1))
                                   identifiers nil)
                   do (push (flycheck-error-new-at (line-number-at-pos) (match-beginning 0)
                                                   'error
                                                   (format "Link target %s does not exist" url)
                                                   :checker checker)
                            errors)))))
    (funcall callback 'finished errors)))

(flycheck-define-generic-checker 'adq/markdown-internal-links
  "Checker for finding internal links with missing targets."
  :start #'adq/markdown-check-internal-links
  :modes '(markdown-mode))

(defun adq/markdown-setup-internal-link-checker ()
  "Enable Flycheck checker for finding internal links with
missing targets."
  (interactive)
  (add-to-list 'flycheck-checkers 'adq/markdown-internal-links t))

(defun adq/markdown-setup ()
  (when (locate-library "pandoc-mode")
    (pandoc-mode 1))
  (yas-minor-mode)
  (when (adq/pandoc-bibliography-present-p)
    (setq-local adq/pandoc-pdf-from-buffer-use-citeproc t)))

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
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'adq/markdown-setup)
  :bind (:map markdown-mode-map
              ("C-c a a" . adq/pandoc-pdf-from-buffer)
              ("C-c a o" . adq/pandoc-pdf-open-target))
  :config
  (setq markdown-bold-underscore t
        markdown-enable-math t
        markdown-command "pandoc")
  (cl-loop for i from 1 to 6
           do (set-face-attribute
               (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
  (set-face-attribute 'markdown-blockquote-face nil :slant 'italic)
  (advice-add 'markdown-follow-link-at-point :around #'adq/markdown-follow-link-at-point-wrap))

(use-package pandoc-mode
  :if (adq/programs-p "pandoc")
  :ensure t
  :defer t)

(provide 'lang-markdown)

;;; lang-markdown.el ends here
