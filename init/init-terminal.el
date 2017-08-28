;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

(use-package multi-term
  :ensure t
  :defer t)

(defun get-multi-term ()
  "Switch to an existing terminal buffer or open a new one."
  (interactive)
  (switch-or-call
   (compose 'car (partial buffers-with-major-mode 'term-mode))
   'multi-term))

(defun get-eshell ()
  "Switch to an existing eshell buffer or open a new one."
  (interactive)
  (switch-or-call
   (compose 'car (partial buffers-with-major-mode 'eshell-mode))
   'eshell))

(defvar prefer-eshell t
  "Should command that launch or retrieve terminals prefer eshell over multi-term.")

(defun get-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if prefer-eshell
       (switch-command (get-eshell) (get-multi-term))
     (switch-command (get-multi-term) (get-eshell)))
   d))

(defun new-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if prefer-eshell
       (switch-command (eshell) (multi-term))
     (switch-command (multi-term) (eshell)))
   d))

(bind-key
 "C-c <return>" #'get-shell-like)
(bind-key
 "C-c <C-return>" #'new-shell-like)

(use-package em-term
  :config
  (add-to-list-many 'eshell-visual-commands '("most"))
  (add-to-list-many 'eshell-visual-subcommands '(("git" ("log" "diff" "show")))))

(provide 'init-terminal)
