;;; init-check.el --- Syntax Checking -*- lexical-binding: t -*-

(use-package flycheck
  :commands (flycheck-mode)
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq
   flycheck-mode-line-prefix "✔"
   flycheck-mode-line
   '(:eval
     (if (eq flycheck-last-status-change 'no-checker)
         ""
       (flycheck-mode-line-status-text)))))

(use-package wcheck-mode
  :if (programs-p "enchant")
  :defer t
  :ensure t
  :config
  (defvar finnish-syntax-table
    (copy-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?- "w" finnish-syntax-table)
  (let ((enchant (executable-find "enchant")))
    (setq wcheck-language-data
          `(("Finnish"
             (program . ,enchant)
             (args "-l" "-d" "fi")
             (syntax . finnish-syntax-table)
             (action-program . ,enchant)
             (action-args "-a" "-d" "fi")
             (action-parser . wcheck-parser-ispell-suggestions))
            ("American English"
             (program . ,enchant)
             (args "-l" "-d" "en_US")
             (action-program . ,enchant)
             (action-args "-a" "-d" "en_US")
             (action-parser . wcheck-parser-ispell-suggestions))))))

(use-package guess-language
  :defer t
  :ensure t)

(use-package guess-language-lite
  :commands (gll-guess-language-lite-mode)
  :config
  (setq guess-language-languages '(en fi))
  (when (programs-p "enchant")
    (add-hook 'gll-language-identified-functions #'guess-language-wcheck-hook)))

(defvar guess-language-code-to-wcheck-name-map
  '((fi . "Finnish")
    (en . "American English")))

;; Rename to wcheck hook and only add if available
(defun guess-language-wcheck-hook (lang)
  (let ((current-code (car (rassoc 'wcheck-language
                                   guess-language-code-to-wcheck-name-map))))
    (unless (eq current-code lang)
      (let ((lang-name (cdr (assoc lang
                                   guess-language-code-to-wcheck-name-map))))
        (wcheck-change-language lang-name)))))


(provide 'init-check)
