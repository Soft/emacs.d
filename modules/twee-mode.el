;;; twee-mode.el ---- Major modes for editing twee files -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Major modes for editing twee files.
;; This mode supports a small sub-set of SugarCube's syntax.

;;; Code:

(require 'rx)

(defgroup twee-mode nil
  "Major mode for twee files."
  :group 'data
  :prefix "twee-mode-")

(defface twee-mode-passage-prefix-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for passage prefixes."
  :group 'twee-mode)

(defface twee-mode-passage-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for passage names."
  :group 'twee-mode)

(defface twee-mode-link-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for links."
  :group 'twee-mode)

(defface twee-mode-link-label-face
  '((t (:inherit font-lock-string-face)))
  "Face for link label."
  :group 'twee-mode)

(defface twee-mode-heading-prefix-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for heading prefixes."
  :group 'twee-mode)

(defface twee-mode-heading-text-face
  '((t (:inherit font-lock-string-face)))
  "Face for heading texts."
  :group 'twee-mode)

(defface twee-mode-list-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for lists."
  :group 'twee-mode)

(defface twee-mode-variable-sigil-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for variable sigils."
  :group 'twee-mode)

(defface twee-mode-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for variable names."
  :group 'twee-mode)

(defface twee-mode-macro-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for macros."
  :group 'twee-mode)

(defface twee-mode-macro-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for macro names."
  :group 'twee-mode)

(defface twee-mode-emphasis-face
  '((t (:slant italic)))
  "Face for emphasis."
  :group 'twee-mode)

(defface twee-mode-strong-face
  '((t (:weight bold)))
  "Face for strong."
  :group 'twee-mode)

(defface twee-mode-underline-face
  '((t (:underline t)))
  "Face for underline."
  :group 'twee-mode)


(defvar twee-mode-passage-header-regexp
  (rx line-start
      (group "::")
      (+ blank)
      (group (+ not-newline))))

(defvar twee-mode-link-regexp
  (rx (group "[[")
      (optional
       (group (+ (not (or "|" "]"))))
       (group "|"))
      (group (+ (not "]")))
      (group "]")
      (optional
       (group "[")
       (* (not "]"))
       (group "]"))
      (group "]")))

(defvar twee-mode-heading-regexp
  (rx line-start
      (group (repeat 1 6 "!"))
      (group (+ not-newline))))

(defvar twee-mode-list-regexp
  (rx line-start
      (group (or "*" "#"))
      (+ blank)
      (+ not-newline)))

(defvar twee-mode-variable-regexp
  (rx
   (group "$")
   (group alpha (* alnum))))

(defvar twee-mode-macro-start-regexp
  (rx
   (group "<<")
   (group (or
           (seq alpha (* alnum))
           "="
           "-"))
   (* (not ">"))
   (group ">>")))

(defvar twee-mode-macro-end-regexp
  (rx
   (group "<</")
   (group (seq alpha (* alnum)))
   (group ">>")))

(defvar twee-mode-emphasis-regexp
  (rx
   (group
    "//"
    (+ (not "/"))
    "//")))

(defvar twee-mode-strong-regexp
  (rx
   (group
    "''"
    (+ (not "'"))
    "''")))

(defvar twee-mode-underline-regexp
  (rx
   (group
    "__"
    (+ (not "_"))
    "__")))

(defvar twee-mode-font-lock-keywords
  `((,twee-mode-passage-header-regexp
     (1 'twee-mode-passage-prefix-face)
     (2 'twee-mode-passage-name-face))
    (,twee-mode-link-regexp
     (1 'twee-mode-link-face)
     (2 'twee-mode-link-label-face nil t)
     (3 'twee-mode-link-face nil t)
     (4 'twee-mode-passage-name-face)
     (5 'twee-mode-link-face)
     (6 'twee-mode-link-face nil t)
     (7 'twee-mode-link-face nil t)
     (8 'twee-mode-link-face))
    (,twee-mode-heading-regexp
     (1 'twee-mode-heading-prefix-face)
     (2 'twee-mode-heading-text-face))
    (,twee-mode-list-regexp
     (1 'twee-mode-list-face))
    (,twee-mode-variable-regexp
     (1 'twee-mode-variable-sigil-face)
     (2 'twee-mode-variable-name-face))
    (,twee-mode-macro-start-regexp
     (1 'twee-mode-macro-face)
     (2 'twee-mode-macro-name-face)
     (3 'twee-mode-macro-face))
    (,twee-mode-macro-end-regexp
     (1 'twee-mode-macro-face)
     (2 'twee-mode-macro-name-face)
     (3 'twee-mode-macro-face))
    (,twee-mode-emphasis-regexp
     (1 'twee-mode-emphasis-face))
    (,twee-mode-strong-regexp
     (1 'twee-mode-strong-face))
    (,twee-mode-underline-regexp
     (1 'twee-mode-underline-face))))

;;;###autoload
(define-derived-mode twee-mode fundamental-mode "Twee"
  "Major modes for editing twee files."
  :group 'twee-mode
  (setq font-lock-defaults '(twee-mode-font-lock-keywords))
  (font-lock-mode 1)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local
   imenu-generic-expression
   (list (list nil twee-mode-passage-header-regexp 2))))

(provide 'twee-mode)

;;; twee-mode.el ends here
