;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Package management

(use-package paradox
  :ensure t
  :defer t
  :commands (paradox-enable))

(use-package package-utils
  :defer t
  :ensure t)

(run-with-idle-timer 1 nil (lambda () (paradox-enable)))

(provide 'init-packages)
