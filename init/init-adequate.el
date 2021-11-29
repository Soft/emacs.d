;;; init-adequate.el -*- lexical-binding: t; -*-

;; Disable unsafe TLS algorithms
(setq gnutls-verify-error t
      gnutls-algorithm-priority "SECURE128:+SECURE192:-VERS-ALL:+VERS-TLS1.3"
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error)

;; Ensure straight.el is installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defconst adq/emacs-use-package-collect-statistics
  (and (getenv "EMACS_PACKAGE_STATS") t))

(when adq/emacs-use-package-collect-statistics
  (setq use-package-compute-statistics t))

(setq straight-use-package-by-default t)

(dolist (subdir '("init" "config" "env" "modules"))
  (add-to-list 'load-path (concat adq/adequate-directory subdir)))

(use-package diminish)

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :diminish gcmh-mode)

(use-package no-littering
  :config
  (setq custom-file
        (concat no-littering-etc-directory "customize.el")))

(defmacro adq/init (&rest modules)
  (let ((requires
         (mapcar (lambda (module)
                   `(require (quote ,module) nil t))
                 modules)))
    `(let ((gc-cons-threshold most-positive-fixnum)
           (file-name-handler-alist nil))
       ,@requires)))

(provide 'init-adequate)
