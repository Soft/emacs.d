;;; portage-mode.el ---- Portage support for Emacs -*- lexical-binding: t -*-

;; Author: Samuel Laurén <samuel.lauren@iki.fi>

;;; Commentary:

;; Major modes for editing Portage's various configuration files. Adds syntax
;; highlighting and useful utility functions for working with configuration
;; files.

;; Currently, the package provides three major modes:

;; `portage-mode', a generic mode for working with Portage configuration
;; `portage-mode-use-mode', a mode for working with package.use files
;; `portage-mode-accept-keywords-mode', a mode for working with package.accept_keywords files

;; For use with `use-packge':
;;
;; (use-package portage-mode
;;   :mode (("/package\\.use" . portage-mode-use-mode)
;;          ("/package\\.accept_keywords" . portage-mode-accept-keywords-mode)
;;          ((rx "/package." (or "env" "license" "mask" "unmask")) . portage-mode)))

;;; Code:

(require 'rx)
(require 'subr-x)

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

(defface portage-mode-overlay-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for overlay specifications."
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
  '((t (:inherit font-lock-type-face)))
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

;; Regexp for matching atoms

(defvar portage-mode-atom-regexp
  (rx
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
           (0+ (any digit ?.))))
   (optional ; Overlay specification
    (group "::")
    (group (any alpha)
           (0+ (any alnum ?_ ?.)
               (0+ (seq "-"
                        (any alnum ?_ ?.) (0+ (any alnum ?_ ?.))))))))
  "Regular expression used for matching atoms.")

(defvar portage-mode-font-lock-keywords-atom
  `(,portage-mode-atom-regexp
    (1 'portage-mode-extended-prefix-operator-face nil t)
    (2 'portage-mode-prefix-operator-face nil t)
    (3 'portage-mode-category-face)
    (4 'portage-mode-separator-face)
    (5 'portage-mode-package-face)
    (6 'portage-mode-separator-face nil t)
    (7 'portage-mode-version-face nil t)
    (8 'portage-mode-separator-face nil t)
    (9 'portage-mode-slot-face nil t)
    (10 'portage-mode-separator-face nil t)
    (11 'portage-mode-overlay-face nil t))
  "Font lock rule for highlighting package atoms.")

;; FIXME: Is there something wrong with special-arch

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
         (group "-" (not (any blank ?-)) (* (not (any blank))))
         (group (not (any blank ?-)) (* (not (any blank))))))
      nil nil
      (1 'portage-mode-use-disabled-face nil t)
      (2 'portage-mode-use-enabled-face nil t))))
  "Font lock rules for package.use files.")


(defvar portage-mode-generic-font-lock-keywords
  (list portage-mode-font-lock-keywords-atom)
  "Font lock rules for generic Portage config modes.")


(defvar portage-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (dolist (c '(?: ?. ?- ?/ ?* ?~ ?_))
      (modify-syntax-entry c "_" table))
    table)
  "Syntax table for Portage files.")

;; Convenience commands

(defun portage-mode-current-atom ()
  (save-match-data
    (if-let ((begin (save-excursion
                      (and (re-search-backward (rx (or line-start blank)) nil t)
                           (point))))
             (end (save-excursion
                    (and (re-search-forward (rx (or line-end blank)) nil t)
                         (point)))))
        (cons begin end))))

;;;###autoload
(defun portage-mode-simplify-atom-at-pt ()
  "Clean package atom at point from any constraints if might
have (version specifiers etc.) leaving only the
category/package-name pair intact.

For example >=dev-qt/qtgui-5.6.1 becomes dev-qt/qtgui"
  (interactive)
  (if-let ((region (portage-mode-current-atom))
           (string (buffer-substring-no-properties (car region) (cdr region)))
           (match (string-match portage-mode-atom-regexp string))
           (package-category (match-string 3 string))
           (package-name (match-string 5 string)))
      (progn
        (delete-region (car region) (cdr region))
        (insert (format
                 "%s/%s "
                 package-category
                 package-name)))
    (error "No package atom at point.")))

;; Mode definitions

;;;###autoload
(define-derived-mode portage-mode conf-mode "Portage"
  "Major mode for editing Portage's config files."
  (setq font-lock-defaults '(portage-mode-generic-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table))

;;;###autoload
(define-derived-mode portage-mode-accept-keywords-mode conf-mode "Portage/Keywords"
  "Major mode for editing Portage's package.accept_keywords files."
  (setq font-lock-defaults '(portage-mode-accept-keywords-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table))

;;;###autoload
(define-derived-mode portage-mode-use-mode conf-mode "Portage/USE"
  "Major mode for editing Portage's package.use files."
  (setq font-lock-defaults '(portage-mode-use-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table))


(provide 'portage-mode)

