;;; portage-mode.el ---- Major modes for editing Portage's configuration files -*- lexical-binding: t -*-

;; Copyright (C) 2017 Samuel Laurén

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Keywords: Portage, Gentoo, USE Flags, Configuration
;; Package-Requires: ((emacs "25") (async))

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

;; Major modes for editing Portage's configuration files. Adds syntax
;; highlighting and useful convenience functions for working with configuration
;; files.

;; The package provides three major modes:

;; - `portage-mode', a generic mode for working with Portage configuration
;; - `portage-mode-use-mode', a mode for working with package.use files
;; - `portage-mode-accept-keywords-mode', a mode for working with
;;   package.accept_keywords files

;; # Eldoc support

;; `portage-mode-use-mode' provides eldoc support when the user has equery is
;; installed. This can be disabled by setting `portage-mode-use-mode-want-eldoc'
;; to nil. By default, eldoc will display all available USE flags for the
;; package in current line.

;; # FlyCheck support

;; `portage-mode-use-mode' includes a checker for invalid USE flags. Flags that
;; are requested but that are not supported by the package in question are
;; marked as errors.

;; # Imenu support

;; All Portage modes support imenu by allowing quick jumping between package
;; entries.

;; For use with `use-packge':
;;
;; (use-package portage-mode
;;   :mode (("/package\\.use" . portage-mode-use-mode)
;;          ("/package\\.accept_keywords" . portage-mode-accept-keywords-mode)
;;          ((rx "/package." (or "env" "license" "mask" "unmask")) . portage-mode)))

;;; Code:

(require 'rx)
(require 'subr-x)
(require 'async)
(require 'diff-mode) ;; For faces
(require 'eldoc)
(require 'cl-lib)
(require 'flycheck)

(defgroup portage-mode nil
  "Major mode for Portage files."
  :group 'data
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

(defvar portage-mode-equery-binary "equery"
  "Name of equery binary.")

(defvar portage-mode-use-mode-want-eldoc t
  "Should eldoc mode be enabled in `portage-mode-eldoc-use-mode'")

(defvar portage-mode-use-flag-cache (make-hash-table :test 'equal))

(defvar portage-mode-metadata-cache (make-hash-table :test 'equal))

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

(defvar portage-mode-use-flag-regexp
  (rx
   (or
    (group "-" (not (any blank ?-)) (* (not (any blank))))
    (group (not (any blank ?-)) (* (not (any blank))))))
  "Regular expression used for matching USE flags.")

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
     (,portage-mode-use-flag-regexp
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

(defun portage-mode-bounds-of-atom-at-point ()
  (save-match-data
    (if-let ((begin (save-excursion
                      (and (re-search-backward (rx (or line-start blank)) nil t)
                           (point))))
             (end (save-excursion
                    (and (re-search-forward (rx (or line-end blank)) nil t)
                         (point)))))
        (cons begin end))))

(defun portage-mode-atom-at-current-line (&optional properties)
  "Returns package atom from the current line or nil if there is
no package atom on the line. If optional PROPERTIES is non-nil,
the returned string will have its associated string properties."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward portage-mode-atom-regexp (point-at-eol) t)
        (funcall (if properties
                     'match-string
                   'match-string-no-properties)
                 0)))))

;;;###autoload
(defun portage-mode-simplify-atom-at-point ()
  "Clean package atom at point from any constraints if might
have (version specifiers etc.) leaving only the
category/package-name pair intact.

For example >=dev-qt/qtgui-5.6.1 becomes dev-qt/qtgui"
  (interactive)
  (if-let ((location (point))
           (region (portage-mode-bounds-of-atom-at-point))
           (string (buffer-substring-no-properties (car region) (cdr region)))
           (match (string-match portage-mode-atom-regexp string))
           (package-category (match-string 3 string))
           (package-name (match-string 5 string)))
      (progn
        (delete-region (car region) (cdr region))
        (insert (format
                 "%s/%s "
                 package-category
                 package-name))
        (goto-char location))
    (error "No package atom at point.")))

(defun portage-mode-browser-for-atom-at-current-line ()
  "Open file browser with location of atoms ebuilds."
  (interactive)
  (if-let ((atom (portage-mode-atom-at-current-line)))
      (portage-mode-metadata-for-atom
       atom
       (lambda (atom meta)
         (if-let ((location (assq 'location meta)))
             (browse-url (cadr location)))))))

(defun portage-mode-parse-equery-u-output (buffer)
  "Parse USE flags from buffer containing 'equery u ATOM'
output."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((flags '()))
        (while (not (eobp))
          (push
           (cons (if (equal (buffer-substring (point)
                                              (+ 1 (point))) "+")
                     t nil)
                 (buffer-substring (+ 1 (point)) (point-at-eol)))
           flags)
          (beginning-of-line 2))
        (nreverse flags)))))

(defun portage-mode-parse-equery-m-output (buffer)
  "Parse package metadata from buffer containing 'equery m ATOM'
output."
  (with-current-buffer buffer
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (let ((meta '()))
          (while (not (eobp))
            (re-search-forward
             (rx line-start
                 (group (1+ (not (any blank))))
                 (* blank)
                 (group (1+ not-newline)))
             (point-at-eol) t)
            (let* ((key (pcase (match-string-no-properties 1)
                          ("Maintainer:" 'maintainer)
                          ("Location:" 'location)
                          ("Homepage:" 'homepage)
                          ("Keywords:" 'keywords)
                          ("License:" 'license)))
                   (current (and key (assq key meta))))
              (when key
                (if current
                    (push (match-string-no-properties 2) (cdr current))
                  (push (cons key (list (match-string-no-properties 2))) meta)))) 
            (beginning-of-line 2))
          meta)))))

(defun portage-mode-call-equery-async (subcommand parser atom success-callback &optional failure-callback cache)
  (if-let ((cached (and cache (gethash atom cache))))
      (funcall success-callback atom cached)
    (async-start-process
     (format "equery %s %s" subcommand atom) portage-mode-equery-binary
     (lambda (proc) ;; NOTE: If execution fails we do not cache anything
       (if (and (eq (process-status proc) 'exit)
                (= (process-exit-status proc) 0))
           (let ((parsed (funcall parser (process-buffer proc))))
             (when cache
               (puthash atom parsed cache))
             (funcall success-callback atom parsed))
         (when failure-callback
           (funcall failure-callback atom proc))))
     "-C" subcommand atom)))

(defun portage-mode-use-flags-for-atom (atom success-callback &optional failure-callback)
  "Retrieve list of USE flags supported by ATOM. The process is
asynchronous and SUCCESS-CALLBACK will be called with the atom
and a list of supported USE flags.

If equery fails FAILURE-CALLBACK will be called with the atom and
the process object."
  (portage-mode-call-equery-async "u" #'portage-mode-parse-equery-u-output atom
                                  success-callback failure-callback portage-mode-use-flag-cache))

(defun portage-mode-metadata-for-atom (atom success-callback &optional failure-callback)
  (portage-mode-call-equery-async "m" #'portage-mode-parse-equery-m-output atom
                                  success-callback failure-callback portage-mode-metadata-cache))

;; Eldoc support

(defun portage-mode-use-flags-eldoc-function ()
  (if-let ((atom (portage-mode-atom-at-current-line t)))
      (portage-mode-use-flags-for-atom
       atom
       (lambda (atom flags)
         (eldoc-message (portage-mode-format-use-flags atom flags))))))

(defun portage-mode-format-use-flags (atom flags)
  (format "%s: %s"
          atom
          (string-join
           (mapcar (lambda (flag)
                     (propertize (cdr flag)
                                 'face
                                 (if (car flag)
                                     'portage-mode-use-enabled-face
                                   'portage-mode-use-disabled-face)))
                   flags)
           " ")))

;; Flycheck support

(defmacro portage-mode-decrement-and-call-when-zero (place fn &rest args)
  `(progn
     (decf,place)
     (when (zerop ,place)
       (apply (quote,fn) (list ,@args)))))

(defun portage-mode-start-use-flag-check (checker callback)
  "Checker function for Flycheck. The checker checks buffer for
  packages with USE flags that are not available for the
  package."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((counter 1) ;; There is a possibility of race condition here in the way this variable is modified
            (line 1)
            (errors '()))
        (while (not (eobp))
          (when (re-search-forward portage-mode-atom-regexp (point-at-eol) t)
            (let* ((atom (match-string-no-properties 0))
                   (flags '()))
              (while (re-search-forward portage-mode-use-flag-regexp (point-at-eol) t)
                (push (cons (- (match-end 0) (line-beginning-position))
                            (string-remove-prefix "-" (match-string-no-properties 0)))
                      flags))
              (incf counter)
              (portage-mode-use-flags-for-atom
               atom
               (lambda (atom supported-flags)
                 (dolist (flag flags)
                   (unless (member (cdr flag) (mapcar #'cdr supported-flags))
                     (push (flycheck-error-new-at line (car flag) 'error
                                                  (format "%s does not support %s flag" atom (cdr flag))
                                                  :checker checker)
                           errors)))
                 (portage-mode-decrement-and-call-when-zero
                  counter portage-mode-finish-use-flag-check callback errors)))))
          (beginning-of-line 2)
          (incf line))
        (portage-mode-decrement-and-call-when-zero
         counter portage-mode-finish-use-flag-check callback errors)))))

(defun portage-mode-finish-use-flag-check (callback errors)
  (funcall callback 'finished errors))

(flycheck-define-generic-checker 'portage-mode-use-flags
  "Checker for detecting invalid USE flags."
  :start #'portage-mode-start-use-flag-check
  :modes '(portage-mode-use-mode))

(defun portage-mode-setup-use-flag-checker ()
  (interactive)
  (add-to-list 'flycheck-checkers 'portage-mode-use-flags t))

;; Major modes

;;;###autoload
(define-derived-mode portage-mode fundamental-mode "Portage"
  "Major mode for editing Portage's config files."
  :group 'portage-mode
  (setq font-lock-defaults '(portage-mode-generic-font-lock-keywords))
  (font-lock-mode 1)
  (set-syntax-table portage-mode-syntax-table)
  (setq-local imenu-generic-expression
              (list (list nil portage-mode-atom-regexp 0))))

;;;###autoload
(define-derived-mode portage-mode-accept-keywords-mode portage-mode "Portage/Keywords"
  "Major mode for editing Portage's package.accept_keywords files."
  (setq font-lock-defaults '(portage-mode-accept-keywords-font-lock-keywords)))

;;;###autoload
(define-derived-mode portage-mode-use-mode portage-mode "Portage/USE"
  "Major mode for editing Portage's package.use files."
  (setq font-lock-defaults '(portage-mode-use-font-lock-keywords))
  (when (and (executable-find portage-mode-equery-binary)
             portage-mode-use-mode-want-eldoc)
    (set (make-local-variable 'eldoc-documentation-function)
         #'portage-mode-use-flags-eldoc-function)))


(provide 'portage-mode)

