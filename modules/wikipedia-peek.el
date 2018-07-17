;;; wikipedia-peek.el --- View article summaries from Wikipedia -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (request))

;;; Commentary:

;; View article summaries from Wikipedia.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'org)

(defvar wikipedia-peek-language "en"
  "Two-letter language code specifying Wikipedia version.")

(defvar wikipedia-peek-render-html t
  "Should wikipedia-peek render HTML.")

(defvar wikipedia-peek-show-images t
  "Should wikipedia-peek show images.")

(defvar-local wikipedia-peek--page-url nil
  "Article URL")

(defvar wikipedia-peek-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "<escape>" #'quit-window)
    (define-key map "o" #'wikipedia-peek-browse-url)
    map)
  "Keymap for `wikipedia-peek-mode'.")

(defface wikipedia-peek-title-face
  '((t (:inherit org-level-1)))
  "Face for titles in `wikipedia-peek-mode'.")

(defface wikipedia-peek-description-face
  '((t (:inherit org-verse)))
  "Face for descriptions in `wikipedia-peek-mode'.")

(defun wikipedia-peek-browse-url ()
  (interactive)
  (if wikipedia-peek--page-url
      (browse-url wikipedia-peek--page-url)
    (error "No page URL.")))

(defun wikipedia-peek--api-url (article)
  (format "https://%s.wikipedia.org/api/rest_v1/page/summary/%s"
          wikipedia-peek-language
          (url-hexify-string article)))

(defmacro wikipedia-peek--enable-editing (&rest body)
  `(progn
     (setq-local buffer-read-only nil)
     (unwind-protect
         (progn
           ,@body)
       (setq-local buffer-read-only t))))

(defun wikipedia-peek--get-buffer (name)
  (if-let ((buffer (cl-loop for buffer being the buffers
                            if (with-current-buffer buffer
                                 (eq major-mode 'wikipedia-peek-mode))
                            return buffer)))
      (progn
        (with-current-buffer buffer
          (rename-buffer name)
          buffer))
    (get-buffer-create name)))

(cl-defun wikipedia-peek--handle-success (&key data &allow-other-keys)
  (let-alist data
    (let ((buffer (wikipedia-peek--get-buffer (format "*Wikipedia: %s*" .title))))
      (with-current-buffer buffer
        (wikipedia-peek--enable-editing
         (unless (eq major-mode 'wikipedia-peek-mode)
           (wikipedia-peek-mode))
         (erase-buffer)
         (setq-local wikipedia-peek--page-url .content_urls.desktop.page)
         (when .displaytitle
           (insert (propertize .displaytitle 'face 'wikipedia-peek-title-face) ?\n))
         (when .description
           (insert (propertize .description 'face 'wikipedia-peek-description-face) ?\n ?\n))
         (if wikipedia-peek-render-html
             (progn
               (insert .extract_html)
               (shr-render-region (point-min) (point-max)))
           (insert .extract)))
        (when (and wikipedia-peek-show-images .thumbnail .thumbnail.source)
          (request .thumbnail.source
                   :parser 'buffer-string
                   :success (lambda (&rest args)
                              (apply #'wikipedia-peek--handle-image buffer args))
                   :error #'wikipedia-peek--handle-error))
        (pop-to-buffer buffer)))))

(cl-defun wikipedia-peek--handle-image (buffer &key data &allow-other-keys)
  (when (and data (buffer-live-p buffer))
    (with-current-buffer buffer
      (wikipedia-peek--enable-editing
       (goto-char (point-max))
       (insert ?\n ?\n)
       (insert-image (create-image (string-as-unibyte data) nil t))
       (insert ?\n)))))

(defun wikipedia-peek--handle-error (&rest _)
  (error "wikipedia-peek: error while processing request."))

(defun wikipedia-peek--make-request (article)
  (request (wikipedia-peek--api-url article)
           :parser 'json-read
           :success #'wikipedia-peek--handle-success
           :error #'wikipedia-peek--handle-error))

(define-derived-mode wikipedia-peek-mode special-mode "Wikipedia"
  "Major mode for viewing article summaries from Wikipedia."
  (font-lock-mode 1))

;;;###autoload
(defun wikipedia-peek (article)
  (interactive "sArticle: ")
  (wikipedia-peek--make-request article))

(provide 'wikipedia-peek)

;;; wikipedia-peek.el ends here
