;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Movement and selection related configuration

(use-package avy
  :defer t
  :ensure t)

;; This doesn't really seem to work the way I want. Maybe I'll switch to
;; something else.
(use-package smart-forward
  :commands (smart-backward smart-forward)
  :defer t
  :ensure t)

(use-package expand-region
  :defer t
  :ensure t)

(use-package goto-last-change
  :ensure t
  :bind (("C-c ." . goto-last-change)
         ("C-c C-." . goto-last-change-reverse)))

(provide 'init-movement)
