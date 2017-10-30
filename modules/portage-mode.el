;;; portage-mode.el ---- Portage support for Emacs -*- mode: Emacs-Lisp; lexical-binding: t; -*-

;; Author: Samuel Laurén <samuel.lauren@iki.fi>

;;; Commentary:

;;; Code:

(require 'rx)

(defgroup portage-mode nil
  "Major mode for Portage files."
  :prefix "portage-mode-")

;; Faces for package atom highlights.

(defface portage-mode-extended-prefix-operator-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for extended prefix operators."
  :group 'portage-mode)

(defface portage-mode-prefix-operator-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for prefix operators."
  :group 'portage-mode)

(defface portage-mode-category-face
  '((t (:inherit font-lock-type-face)))
  "Face for package categories."
  :group 'portage-mode)

(defface portage-mode-package-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for package names."
  :group 'portage-mode)

(defface portage-mode-version-face
  '((t (:inherit font-lock-constant-face)))
  "Face for version strings."
  :group 'portage-mode)

(defface portage-mode-separator-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for separator strings."
  :group 'portage-mode)

(defface portage-mode-slot-face
  '((t (:inherit font-lock-constant-face)))
  "Face for slot strings."
  :group 'portage-mode)

;; Faces for package.accept_keywords specific highlights.

(defface portage-mode-keyword-prefix-face
  '((t (:inherit font-lock-constant-face)))
  "Face for ~ keyword prefix."
  :group 'portage-mode)

(defface portage-mode-arch-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for architectures."
  :group 'portage-mode)

(defface portage-mode-special-arch-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for architectures."
  :group 'portage-mode)

;; Faces for package.use specific highlights.

(defface portage-mode-use-disabled-face
  '((t (:inherit diff-indicator-removed)))
  "Face for disabled USE flags."
  :group 'portage-mode)

(defface portage-mode-use-enabled-face
  '((t (:inherit diff-indicator-added)))
  "Face for enabled USE flags."
  :group 'portage-mode)


(defvar portage-mode-font-lock-keywords-atom
  `(,(rx
      (seq
       line-start
       (group (optional ; Extended prefix operators
               (or "~" "!" "!!" "*")))
       (group (optional ; Prefix operators
               (or ">" ">=" "=" "<=" "<")))
       (group (any alpha) ; Package category
              (0+ (any alnum ?_ ?.)
                  (0+ (seq "-"
                           (any alpha) (0+ (any alnum ?_ ?.))))))
       (group "/") ; Separator between package category and name
       (group (any alpha) ; Package name
              (0+ (any alnum ?_ ?.)
                  (0+ (seq "-"
                           (any alpha) (0+ (any alnum ?_ ?.))))))
       (optional
        (group "-") ; Separator between name and version
        (group (any digit) ; Package version
               (0+ (any alnum ?_ ?.)
                   (0+ (seq "-"
                            (any alnum ?_ ?.) (0+ (any alnum ?_ ?.)))))))
       (optional ; Slot operators
        (group ":") ; TODO: Support sub-slots
        (group (any digit)
               (0+ (any digit ?.))))))
    (1 'portage-mode-extended-prefix-operator-face nil t)
    (2 'portage-mode-prefix-operator-face nil t)
    (3 'portage-mode-category-face)
    (4 'portage-mode-separator-face)
    (5 'portage-mode-package-face)
    (6 'portage-mode-separator-face nil t)
    (7 'portage-mode-version-face nil t)
    (8 'portage-mode-separator-face nil t)
    (9 'portage-mode-slot-face nil t))
  "Font lock rule for highlighting package atoms.")


(defvar portage-mode-accept-keywords-font-lock-keywords
  `((,@portage-mode-font-lock-keywords-atom
     (,(rx
        word-start
        (or
         (seq
          (optional
           (group "~"))
          (or (group (1+ alnum))
              (group "*")))
         (group (repeat 1 2 "*")))
        word-end)
      nil nil
      (1 'portage-mode-keyword-prefix-face nil t)
      (2 'portage-mode-arch-face nil t)
      (3 'portage-mode-special-arch-face nil t)
      (4 'portage-mode-special-arch-face nil t))))
  "Font lock rules for package.accept_keywords files.")


(defvar portage-mode-use-font-lock-keywords
  `((,@portage-mode-font-lock-keywords-atom
     (,(rx
        (or
         (group (seq "-" alpha (* (any alnum ?-))))
         (group (seq alpha (* (any alnum ?-))))))
      nil nil
      (1 'portage-mode-use-disabled-face nil t)
      (2 'portage-mode-use-enabled-face nil t))))
  "Font lock rules for package.use files.")


(defvar portage-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for Portage files.")


;;;###autoload
(define-derived-mode portage-accept-keywords-mode conf-mode "Portage Keywords"
  "Major mode for editing Portage's package.accept_keywords files."
  (setq font-lock-defaults '(portage-mode-accept-keywords-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table))

;;;###autoload
(define-derived-mode portage-use-mode conf-mode "Portage USE flags"
  "Major mode for editing Portage's package.use files."
  (setq font-lock-defaults '(portage-mode-use-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table))

(provide 'portage-mode)

