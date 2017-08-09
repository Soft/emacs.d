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

(bind-key
 "C-c <return>" (switch-command (get-multi-term) (get-eshell)))
(bind-key
 "C-c <C-return>" (switch-command (multi-term) (eshell)))

(provide 'init-terminal)
