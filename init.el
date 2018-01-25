;;; init.el --- Adequate emacs.d -*- lexical-binding: t -*-

;; Adequate emacs.d -- Opionated Emacs configuration
;; Copyright (C) 2017 Samuel Laurén

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

;; Samuel Laurén 💗 2014-2017

;;; Code:

;; Check version compatibility

(when (version< emacs-version "25")
  (error "Adequate emacs.d requires Emacs 25"))

;; Alter garbage collection policy for the duration of startup

(let ((default-threshold gc-cons-threshold))
  (setq gc-cons-threshold 64000000)
  (add-hook 'after-init-hook
            #'(lambda ()
                (setq gc-cons-threshold default-threshold))))

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

(defun adq/with-archive-protocol (url)
  (let ((proto (if (getenv "EMACS_NO_TLS") "http" "https")))
    (format "%s://%s" proto url)))

(defvar package-archives
  `(("melpa" . ,(adq/with-archive-protocol "melpa.org/packages/"))
    ("gnu" . ,(adq/with-archive-protocol "elpa.gnu.org/packages/"))))

(require 'package)
(package-initialize)

;; Record package archive refresh times.

(defvar adq/package-last-refresh-time nil
  "Time when the package archive was last refreshed.")

(defun adq/package-update-last-refresh-time (&rest args)
  (setq adq/package-last-refresh-time (current-time)))

(advice-add #'package-refresh-contents :before #'adq/package-update-last-refresh-time)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Initialize use-package

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq load-prefer-newer t)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Debug control

(defconst adq/emacs-debug
  (and (getenv "EMACS_DEBUG") t))

(when adq/emacs-debug
  (setq use-package-verbose t
        debug-on-error t))

;; Benchmark init if we are running with debugging enabled.

(use-package benchmark-init
  :if adq/emacs-debug
  :ensure t
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; Order of modules to load.
;; This is sensitive to changes as there might be dependencies between modules.

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
