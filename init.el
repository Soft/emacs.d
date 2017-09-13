;;; init.el --- Adequate emacs.d -*- lexical-binding: t -*-

;; Samuel Laurén 💗 2014-2017

;;; Commentary:

;; This is my humble attempt at trying to configure Emacs the way I like it.
;; Eternally work in progres.

;;; Code:

;; Alter garbage collection policy for the duration of startup

(let ((default-threshold gc-cons-threshold))
  (setq gc-cons-threshold 64000000)
  (add-hook 'after-init-hook
            #'(lambda ()
                (setq gc-cons-threshold default-threshold))))

(require 'subr-x) ; string-remove-suffix

;; Find the actual location of Adequate emacs.d

(defun get-init-directory ()
  (when load-file-name
    (expand-file-name
     (file-name-directory
      (file-chase-links (string-remove-suffix ".elc" load-file-name))))))

(defconst init-directory (get-init-directory)
  "Location of adequate emacs.d files.")

;; Add core directories into load-path

(dolist (subdir '("init" "languages" "modules"))
  (add-to-list 'load-path (expand-file-name subdir init-directory)))

;; Setup package archives
;; EMACS_NO_TLS environment variable can be used to control if TLS should be used.

(defun with-archive-protocol (url)
  (let ((proto (if (getenv "EMACS_NO_TLS") "http" "https")))
    (format "%s://%s" proto url)))

(defvar package-archives
  `(("melpa" . ,(with-archive-protocol "melpa.milkbox.net/packages/"))
    ("gnu" . ,(with-archive-protocol "elpa.gnu.org/packages/"))))

(require 'package)
(package-initialize)

;; Record package archive refresh times.

(defvar package-last-refresh-time nil
  "Time when the package archive was last refreshed.")

(defun package-update-last-refresh-time (&rest args)
  (setq package-last-refresh-time (current-time)))

(advice-add #'package-refresh-contents :before #'package-update-last-refresh-time)

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

(defconst emacs-debug
  (and (getenv "EMACS_DEBUG") t))

(when emacs-debug
  (setq use-package-verbose t
        debug-on-error t))

;; Benchmark init if we are running with debugging enabled.

(use-package benchmark-init
  :if emacs-debug
  :ensure t
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; Order of modules to load.
;; This is sensitive to changes as there might be dependencies between modules.

(defconst module-load-order
  '((init . (customize
             libraries
             prelude
             packages
             editor
             buffers
             search
             backups
             check
             interface
             dashboard
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
             movement
             xwidget
             neotree
             chat
             pdf
             modeline
             misc
             evil
             space
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

;; Load modules
(dolist (module-class module-load-order)
  (let ((class (symbol-name (car module-class)))
        (modules (cdr module-class)))
    (dolist (module modules)
      (require (intern (concat class "-" (symbol-name module)))))))

;;; init.el ends here
