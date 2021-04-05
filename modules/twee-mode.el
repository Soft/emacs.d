;;; twee-mode.el ---- Major modes for editing twee files -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Major modes for editing twee files.

;;; Code:

(require 'rx)

(defgroup twee-mode nil
  "Major mode for twee files."
  :group 'data
  :prefix "twee-mode-")

(defface twee-mode-passage-header-prefix-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for passage header prefixes."
  :group 'twee-mode)

(defface twee-mode-passage-header-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for passage headers."
  :group 'twee-mode)

(defvar twee-mode-passage-header-regexp
  (rx line-start
      (group "::")
      (+ blank)
      (group (+ not-newline))))

(defvar twee-mode-font-lock-keywords
  `((,twee-mode-passage-header-regexp
     (1 'twee-mode-passage-header-prefix-face)
     (2 'twee-mode-passage-header-face))))

;;;###autoload
(define-derived-mode twee-mode fundamental-mode "Twee"
  "Major modes for editing twee files."
  :group 'twee-mode
  (setq font-lock-defaults '(twee-mode-font-lock-keywords))
  (font-lock-mode 1)
  (setq-local
   imenu-generic-expression
   (list (list nil twee-mode-passage-header-regexp 2))))

(provide 'twee-mode)

;;; twee-mode.el ends here
