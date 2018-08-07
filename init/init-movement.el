;;; init-movement.el --- Movement and selection -*- lexical-binding: t -*-

;;; Commentary:

;; Moving and selecting text effectively is important.

;;; Code:

(use-package avy
  :defer t
  :ensure t
  :bind (("C-c j j" . avy-goto-word-1)
         ("C-c j s" . avy-goto-subword-0)
         ("C-c j m" . avy-goto-symbol-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j c" . avy-goto-char))
  :config
  (adq/after-load 'isearch
    (bind-key "C-j" #'avy-isearch isearch-mode-map)))

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

;;; init-movement.el ends here
