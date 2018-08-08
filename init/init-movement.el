;;; init-movement.el --- Movement and selection -*- lexical-binding: t -*-

;;; Commentary:

;; Moving and selecting text effectively is important.

;;; Code:

(use-package avy
  :defer t
  :ensure t
  :functions (avy--generic-jump)
  :bind (("C-c j j" . avy-goto-word-1)
         ("C-c j s" . avy-goto-subword-0)
         ("C-c j m" . avy-goto-symbol-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j c" . avy-goto-char))
  :config
  (setq avy-style 'pre)
  (adq/after-load 'isearch
    (bind-key "C-j" #'avy-isearch isearch-mode-map)))

(autoload 'avy--generic-jump "avy")

(defmacro adq/avy-jump-command (name doc regex)
  "Make Avy jump command based on a regular expression."
  (declare (indent defun))
  `(defun ,name ()
     ,doc
     (interactive)
     (avy--generic-jump ,regex nil 'pre)))

(adq/avy-jump-command adq/avy-goto-paren
  "Jump to a parenthesis."
  "(\\|)")

(adq/avy-jump-command adq/avy-goto-open-paren
  "Jump to an open parenthesis."
  "(")

(adq/avy-jump-command adq/avy-goto-closing-paren
  "Jump to a closing parenthesis."
  ")")

(adq/avy-jump-command adq/avy-goto-block
  "Jump to a block"
  (rx (or "(" ")" "{" "}" "[" "]")))

(bind-keys
 ("C-c j p" . adq/avy-goto-paren)
 ("C-c j b" . adq/avy-goto-block)
 ("C-c j (" . adq/avy-goto-open-paren)
 ("C-c j )" . adq/avy-goto-closing-paren))

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
