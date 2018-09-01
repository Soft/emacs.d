;;; markdown-internal-links.el --- Flycheck checker for finding internal links with missing targets -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (flycheck) (markdown-mode))

;;; Commentary:

;; Flycheck checker for finding internal links in markdown documents where the
;; link target does not exist.

;; Additionally, patches `markdown-mode' to work with internal links.

;;; Code:

(require 'markdown-mode)
(require 'flycheck)
(require 'subr-x)
(require 'cl-lib)

(defun markdown-internal-links-derive-identifier (title)
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

(defun markdown-internal-links-list-titles ()
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

(defun markdown-internal-links-title-identifiers ()
  "Return hash table with document identifers as keys and
  position as values."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (title (markdown-internal-links-list-titles))
      (puthash (markdown-internal-links-derive-identifier (car title)) (cdr title) hash))
    hash))

;;;###autoload
(defun markdown-internal-links-follow-link-at-point-wrapper (fn)
  (interactive)
  (if (markdown-link-p)
      (let ((link (markdown-link-url)))
        (if (string-prefix-p "#" link)
            (if-let ((position
                      (gethash (markdown-internal-links-derive-identifier
                                (substring link 1))
                               (markdown-internal-links-title-identifiers)
                               nil)))
                (goto-char position)
              (error "Link target does not exist"))
          (call-interactively fn)))
    (error "No link at point")))

;;;###autoload
(defun markdown-internal-links-enable-follow-link-at-point ()
  "Enable `markdown-follow-link-at-point' to follow internal
  links."
  (interactive)
  (advice-add 'markdown-follow-link-at-point
              :around
              #'markdown-internal-links-follow-link-at-point-wrapper))

;;;###autoload
(defun markdown-internal-links-check-links (checker callback)
  "Checker function for Flycheck. The checker checks the buffer
  for internal links that do not have a target."
  (let ((identifiers (markdown-internal-links-title-identifiers))
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
                   unless (gethash (markdown-internal-links-derive-identifier (substring url 1))
                                   identifiers nil)
                   do (push (flycheck-error-new-at (line-number-at-pos) (match-beginning 0)
                                                   'error
                                                   (format "Link target %s does not exist" url)
                                                   :checker checker)
                            errors)))))
    (funcall callback 'finished errors)))

;;;###autoload
(flycheck-define-generic-checker 'markdown-internal-links
  "Checker for finding internal links with missing targets."
  :start #'markdown-internal-links-check-links
  :modes '(markdown-mode))

;;;###autoload
(defun markdown-internal-links-enable-checker ()
  "Enable Flycheck checker for finding internal links with
missing targets."
  (interactive)
  (add-to-list 'flycheck-checkers 'markdown-internal-links t))

(provide 'markdown-internal-links)

;;; markdown-internal-links.el ends here
