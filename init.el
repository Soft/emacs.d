;;; init.el --- Adequate emacs.d -*- lexical-binding: t -*-

;; Adequate emacs.d -- Opionated Emacs configuration
;; Copyright (C) 2019 Samuel Laurén

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Homepage: https://bitbucket.org/Soft/emacs.d

;;; Commentary:

;; This is my humble attempt at trying to configure Emacs the way I like it.
;; Eternally work in progress.

;; Samuel Laurén 💗 2014-2019

;;; Code:

;; Check version compatibility

(when (version< emacs-version "26")
  (error "Adequate emacs.d requires Emacs 26"))

;; Alter garbage collection policy and file name handlers for the duration of
;; startup

(let ((default-threshold gc-cons-threshold)
      (default-file-name-handler-alist file-name-handler-alist)
      (default-gc-cons-percentage gc-cons-percentage))
  (setq gc-cons-threshold 64000000
        file-name-handler-alist nil
        gc-cons-percentage 0.6)
  (add-hook 'after-init-hook
            #'(lambda ()
                (setq gc-cons-threshold default-threshold
                      file-name-handler-alist default-file-name-handler-alist
                      gc-cons-percentage default-gc-cons-percentage))))

(require 'subr-x) ; string-remove-suffix

;; Find the actual location of Adequate emacs.d

(defun adq/get-init-directory ()
  (when load-file-name
    (expand-file-name
     (file-name-directory
      (file-chase-links (string-remove-suffix ".elc" load-file-name))))))

(defconst adq/init-directory (adq/get-init-directory)
  "Location of adequate emacs.d files.")

;; Add core directories into load-path

(dolist (subdir '("init" "languages" "modules"))
  (add-to-list 'load-path (expand-file-name subdir adq/init-directory)))

;; Setup package archives
;; EMACS_NO_TLS environment variable can be used to control if TLS should be used.

(defconst adq/tls-enabled
  (and (gnutls-available-p)
       (not (getenv "EMACS_NO_TLS")))
  "Will TLS be used.")

(unless adq/tls-enabled
  (warn "TLS was disabled."))

(defun adq/tls-add-protocol (url)
  (let ((proto (if adq/tls-enabled "https" "http")))
    (format "%s://%s" proto url)))

(setq
 gnutls-algorithm-priority "SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%PROFILE_MEDIUM"
 gnutls-min-prime-bits 1024
 gnutls-verify-error t
 ;;; network-security-level 'high
 )

(defvar package-archives
  `(("melpa" . ,(adq/tls-add-protocol "melpa.org/packages/"))
    ("gnu" . ,(adq/tls-add-protocol "elpa.gnu.org/packages/"))))

(require 'package)
(package-initialize)

;; Record package archive refresh times.

(defvar adq/package-last-refresh-time nil
  "Time when the package archive was last refreshed.")

(defun adq/package-update-last-refresh-time (&rest _)
  (setq adq/package-last-refresh-time (current-time)))

(advice-add #'package-refresh-contents :before #'adq/package-update-last-refresh-time)

(unless package-archive-contents
  (message "No package archive available, refreshing...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Initialize use-package

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(setq load-prefer-newer t)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Debug control

(defconst adq/emacs-debug
  (and (getenv "EMACS_DEBUG") t))

(if adq/emacs-debug
    (setq use-package-verbose t
          use-package-expand-minimally t
          use-package-compute-statistics t
          debug-on-error t)
  (setq debug-on-error nil))

;; Benchmark init if we are running with debugging enabled.

(use-package benchmark-init
  :if adq/emacs-debug
  :ensure t
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; Order of modules to load.
;; This is sensitive to changes as there can be dependencies between modules.

(defvar adq/module-load-order
  '((init . (customize
             libraries
             prelude
             packages
             editor
             folding
             bookmarks
             buffers
             search
             backups
             check
             interface
             web
             feed
             dashboard
             snippets
             fonts
             theme
             projectile
             helm
             terminal
             windows
             git
             completion
             session
             help
             scratch
             dired
             movement
             debugging
             xwidget
             neotree
             chat
             pdf
             modeline
             misc
             evil
             ;; space ;; I don't really use these
             local))
    (lang . (c
             lisp
             python
             rust
             haskell
             latex
             web
             markdown
             shell
             org
             docker
             linux
             misc)))
  "Adequate emacs.d modules to load.")

(load (expand-file-name "preload" adq/init-directory) nil nil nil t)

;; Load modules
(dolist (module-class adq/module-load-order)
  (let ((class (symbol-name (car module-class)))
        (modules (cdr module-class)))
    (dolist (module modules)
      (require (intern (concat class "-" (symbol-name module)))))))

;;; init.el ends here
