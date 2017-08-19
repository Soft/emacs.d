;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; ebuild-mode

;; TODO:
;; - add highlighting for common constructs
;; - eclass completion

(require 'cl-lib)
(require 'autoinsert)

(defvar ebuild-mode-portage-path
  "/usr/portage"
  "Portage tree location.")

(defun ebuild-mode-licenses ()
  (let ((license-path (expand-file-name "licenses" ebuild-mode-portage-path)))
    (if (file-exists-p license-path)
        (cl-loop for path in (directory-files license-path t)
                 when (file-regular-p path)
                 collect (file-name-nondirectory path))
      '())))

(defun ebuild-mode-eclasses ()
  (let ((eclass-path (expand-file-name "eclass" ebuild-mode-portage-path)))
    (if (file-exists-p eclass-path)
        (cl-loop for path in (directory-files eclass-path t)
                 when (file-regular-p path)
                 collect (file-name-sans-extension (file-name-nondirectory path)))
      '())))

(define-skeleton ebuild-mode-skeleton
  "Template for ebuilds"
  ""
  "# Copyright 1999-" (format-time-string "%Y") " Gentoo Foundation" \n
  "# Distributed under the terms of the GNU General Public License v2" \n \n
  "EAPI=6" \n \n
  "DESCRIPTION=\"" (skeleton-read "Description: ") "\"" \n
  "HOMEPAGE=\"" (skeleton-read "Homepage: ") "\"" \n
  "SRC_URI=\"" (skeleton-read "URI: ") "\"" \n
  "LICENSE=\"" (completing-read "License: " (ebuild-mode-licenses)) "\"" \n
  "SLOT=\"0\"" \n
  "KEYWORDS=\"~x86 ~amd64\"" \n
  "IUSE=\"\"" \n \n
  "DEPEND=\"""\"" \n
  "RDEPEND=\"${DEPEND}\"" \n \n _)

;;;###autoload
(define-derived-mode ebuild-mode sh-mode "ebuild"
  "Major mode for editing ebuild files."
  (setq-local indent-tabs-mode t) ;; devmanual recommends these.
  (setq-local tab-width 4)
  (when (eq (buffer-size) 0)
    (auto-insert)))

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))
(define-auto-insert "\\.ebuild\\'" 'ebuild-mode-skeleton)

(provide 'ebuild-mode)
