;; -*- mode: Emacs-Lisp; lexical-binding: t; coding: utf-8 -*-
;; Help related functions

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence
          '("C-c p" "C-x 4" "C-x 5" "C-x c" "C-c !"
            "C-x r" "C-x a" "C-c n" "C-c w" "C-c o"
            "C-x x" "C-x 8")
          guide-key/recursive-key-sequence-flag t
          guide-key/idle-delay 0.1
          guide-key/popup-window-position 'bottom)
    (guide-key-mode)))

(use-package help-fns+
  :ensure t)

(defconst adequate-url "https://bitbucket.org/Soft/emacs.d/src")

(defun about-adequate-emacs-d ()
  (interactive)
  (browse-url adequate-url))

(define-key-after
  (lookup-key global-map [menu-bar help-menu])
  [adequate-website]
  '("About Adequate emacs.d" . about-adequate-emacs-d)
  'about-gnu-project)

(provide 'init-help)

