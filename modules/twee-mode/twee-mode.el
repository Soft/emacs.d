;;; twee-mode.el ---- Major modes for editing twee files -*- lexical-binding: t -*-

;; Copyright (C) 2021 Samuel Laurén

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Keywords: Twee, SugarCube
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major modes for editing twee files that use SugarCube.

;;; Code:

(require 'rx)

(defgroup twee-mode nil
  "Major mode for twee files."
  :group 'data
  :prefix "twee-mode-")

(defface twee-mode-delimiter-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for delimiters."
  :group 'twee-mode)

(defface twee-mode-passage-prefix-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for passage prefixes."
  :group 'twee-mode)

(defface twee-mode-passage-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for passage names."
  :group 'twee-mode)

(defface twee-mode-link-label-face
  '((t (:inherit font-lock-string-face)))
  "Face for link label."
  :group 'twee-mode)

(defface twee-mode-image-title-face
  '((t (:inherit font-lock-string-face)))
  "Face for image titles."
  :group 'twee-mode)

(defface twee-mode-image-source-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for image sources."
  :group 'twee-mode)

(defface twee-mode-heading-prefix-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for heading prefixes."
  :group 'twee-mode)

(defface twee-mode-heading-text-face
  '((t (:inherit font-lock-string-face)))
  "Face for heading text."
  :group 'twee-mode)

(defface twee-mode-list-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for lists."
  :group 'twee-mode)

(defface twee-mode-blockquote-prefix-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for blockquote prefixes."
  :group 'twee-mode)

(defface twee-mode-blockquote-text-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote text."
  :group 'twee-mode)

(defface twee-mode-rule-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for horizontal rules."
  :group 'twee-mode)

(defface twee-mode-line-continuation-face
  '((t (:inherit font-lock-comment-delimiter-face)))
  "Face for line continuations."
  :group 'twee-mode)

(defface twee-mode-variable-sigil-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for variable sigils."
  :group 'twee-mode)

(defface twee-mode-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for variable names."
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

(defface twee-mode-strikethrough-face
  '((t (:strike-through t)))
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
       (group (+ (not (or "|" "]")))) ; label
       (group "|"))
      (group (+ (not "]"))) ; target passage
      (group "]")
      (optional
       (group "[")
       (* (not "]")) ; setter
       (group "]"))
      (group "]")))

(defvar twee-mode-image-regexp
  (rx (group "[")
      (group "img")
      (group "[")
      (optional
       (group (+ (not (or "|" "]")))) ; title
       (group "|"))
      (group (+ (not "]"))) ; source
      (group "]")
      (optional
       (group "[")
       (group (+ (not "]"))) ; link
       (group "]"))
      (optional
       (group "[")
       (+ (not "]")) ; setter
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

(defvar twee-mode-blockquote-regexp
  (rx line-start
      (group (+ ">"))
      (group (* any))))

(defvar twee-mode-rule-regexp
  (rx line-start
      (group "----")
      line-end))

(defvar twee-mode-line-continuation-regexp
  (rx (or (seq line-start
               (* blank)
               (group "\\"))
          (seq
           (group "\\")
           (* blank)
           line-end))))

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

(defvar twee-mode-strikethrough-regexp
  (rx
   (group
    "=="
    (+ (not "="))
    "==")))

(defvar twee-mode-font-lock-keywords
  `((,twee-mode-passage-header-regexp
     (1 'twee-mode-passage-prefix-face)
     (2 'twee-mode-passage-name-face))
    (,twee-mode-link-regexp
     (1 'twee-mode-delimiter-face)
     (2 'twee-mode-link-label-face nil t)
     (3 'twee-mode-delimiter-face nil t)
     (4 'twee-mode-passage-name-face)
     (5 'twee-mode-delimiter-face)
     (6 'twee-mode-delimiter-face nil t)
     (7 'twee-mode-delimiter-face nil t)
     (8 'twee-mode-delimiter-face))
    (,twee-mode-image-regexp
     (1 'twee-mode-delimiter-face)
     (2 'twee-mode-macro-name-face)
     (3 'twee-mode-delimiter-face)
     (4 'twee-mode-image-title-face nil t)
     (5 'twee-mode-delimiter-face nil t)
     (6 'twee-mode-image-source-face)
     (7 'twee-mode-delimiter-face)
     (8 'twee-mode-delimiter-face nil t)
     (9 'twee-mode-passage-name-face nil t)
     (10 'twee-mode-delimiter-face nil t)
     (11 'twee-mode-delimiter-face nil t)
     (12 'twee-mode-delimiter-face nil t)
     (13 'twee-mode-delimiter-face))
    (,twee-mode-heading-regexp
     (1 'twee-mode-heading-prefix-face)
     (2 'twee-mode-heading-text-face))
    (,twee-mode-list-regexp
     (1 'twee-mode-list-face))
    (,twee-mode-blockquote-regexp
     (1 'twee-mode-blockquote-prefix-face)
     (2 'twee-mode-blockquote-text-face))
    (,twee-mode-rule-regexp
     (1 'twee-mode-rule-face))
    (,twee-mode-line-continuation-regexp
     (1 'twee-mode-line-continuation-face nil t)
     (2 'twee-mode-line-continuation-face nil t))
    (,twee-mode-variable-regexp
     (1 'twee-mode-variable-sigil-face)
     (2 'twee-mode-variable-name-face))
    (,twee-mode-macro-start-regexp
     (1 'twee-mode-delimiter-face)
     (2 'twee-mode-macro-name-face)
     (3 'twee-mode-delimiter-face))
    (,twee-mode-macro-end-regexp
     (1 'twee-mode-delimiter-face)
     (2 'twee-mode-macro-name-face)
     (3 'twee-mode-delimiter-face))
    (,twee-mode-emphasis-regexp
     (1 'twee-mode-emphasis-face))
    (,twee-mode-strong-regexp
     (1 'twee-mode-strong-face))
    (,twee-mode-underline-regexp
     (1 'twee-mode-underline-face))
    (,twee-mode-strikethrough-regexp
     (1 'twee-mode-strikethrough-face))))

(defvar twee-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?% ". 23b" table)
    table)
  "Syntax table for twee-mode.")

;;;###autoload
(define-derived-mode twee-mode fundamental-mode "Twee"
  "Major modes for editing twee files."
  :group 'twee-mode
  (setq font-lock-defaults '(twee-mode-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table twee-mode-syntax-table)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local
   imenu-generic-expression
   (list (list nil twee-mode-passage-header-regexp 2))))

(provide 'twee-mode)

;;; twee-mode.el ends here
