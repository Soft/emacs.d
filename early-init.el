;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      native-comp-deferred-compilation nil
      package-enable-at-startup nil
      package-native-compile t
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      default-frame-alist
      '((vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . -1)
        (fullscreen . maximized)))

(defconst adq/emacs-debug
  (and (getenv "EMACS_DEBUG") t))

(when adq/emacs-debug
  (setq debug-on-error t
        use-package-verbose t))

(require 'subr-x) ;; string-remove-suffix

(defconst adq/adequate-directory
  (when load-file-name
    (expand-file-name
     (file-name-directory
      (file-chase-links
       (string-remove-suffix ".elc" load-file-name)))))
  "Location of Adequate emacs.d files.")

(set-language-environment "UTF-8")
(setq default-input-method nil)
