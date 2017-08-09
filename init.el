;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Samuel Laurén 💗 2014-2017

(setq gc-cons-threshold (* 1024 1024 512)
      gc-cons-percentage 0.5)

(require 'subr-x) ;; string-remove-suffix

(defun get-init-directory ()
  (when load-file-name
    (expand-file-name
     (file-name-directory
      (file-chase-links (string-remove-suffix ".elc" load-file-name))))))

(defconst init-directory (get-init-directory))

(dolist (subdir '("init" "languages" "modules" "local"))
  (add-to-list 'load-path (expand-file-name subdir init-directory)))

(defun with-archive-protocol (url)
  (let ((proto (if (getenv "EMACS_NO_TLS") "http" "https")))
    (format "%s://%s" proto url)))

(defvar package-archives
  `(("melpa" . ,(with-archive-protocol "melpa.milkbox.net/packages/"))
    ("gnu" . ,(with-archive-protocol "elpa.gnu.org/packages/"))))

(require 'package)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(when (getenv "EMACS_DEBUG")
  (setq use-package-verbose t))

(defconst module-load-order
  '((init . (customize
             libraries
             prelude
             editor
             backups
             check
             interface
             fonts
             theme
             projectile
             helm
             terminal
             packages
             windows
             git
             completion
             session
             help
             movement
             xwidget
             neotree
             modeline
             misc
             evil
             space))
    (lang . (lisp
             python
             rust
             haskell
             latex
             web
             markdown
             shell))))

(dolist (module-class module-load-order)
  (let ((class (symbol-name (car module-class)))
        (modules (cdr module-class)))
    (dolist (module modules)
      (require (intern (concat class "-" (symbol-name module)))))))

