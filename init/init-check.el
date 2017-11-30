;;; init-check.el --- Syntax Checking -*- lexical-binding: t -*-

;;; Commentary:

;; This is kind of a mess right now since I haven't had time to truly dig into
;; flycheck configuration. Additionally, this has some specific customizations
;; for supporting Finnish spell checking. Maybe someday I'll move Finnish
;; language support into a more separate component.

;; TODO: Only use wcheck for Finnish

;;; Code:

(use-package flycheck
  :commands (flycheck-mode)
  :ensure t
  :config
  (fringe-helper-define
    'flycheck-fringe-bitmap-double-arrow nil
    "...XX..."
    "..XXXX.."
    "..XXXX.."
    "...XX..."
    "...XX..."
    "........"
    "...XX..."
    "...XX...") 
  ;; There seems to be something wrong with rust-cargo and rust checkers
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc rust-cargo rust))
  (setq
   flycheck-mode-line-prefix "✔"
   flycheck-mode-line
   '(:eval
     (if (eq flycheck-last-status-change 'no-checker)
         ""
       (flycheck-mode-line-status-text))))
  (bind-keys
   :map flycheck-error-list-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  ;; Redefine flycheck prefix to be C-c e as C-c ! is quite awkward on nordic
  ;; keyboards
  (define-key
    flycheck-mode-map
    flycheck-keymap-prefix
    nil)
  (setq flycheck-keymap-prefix (kbd "C-c e"))
  (define-key
    flycheck-mode-map
    flycheck-keymap-prefix
    flycheck-command-map))

(use-package wcheck-mode
  :if (adq/programs-p "enchant")
  :defer t
  :ensure t
  :config
  (defvar adq/finnish-syntax-table
    (copy-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?- "w" adq/finnish-syntax-table)
  (let ((enchant (executable-find "enchant")))
    (setq wcheck-language-data
          `(("Finnish"
             (program . ,enchant)
             (args "-l" "-d" "fi")
             (syntax . adq/finnish-syntax-table)
             (action-program . ,enchant)
             (action-args "-a" "-d" "fi")
             (action-parser . wcheck-parser-ispell-suggestions))
            ("American English"
             (program . ,enchant)
             (args "-l" "-d" "en_US")
             (action-program . ,enchant)
             (action-args "-a" "-d" "en_US")
             (action-parser . wcheck-parser-ispell-suggestions))))))

(use-package flyspell
  :defer t
  :config
  (setq flyspell-mode-line-string " 🅕")
  (adq/after-load 'helm
    (use-package flyspell-correct-helm
      :ensure t
      :init
      (require 'flyspell-correct-helm)
      (bind-key
       "C-c =" #'flyspell-correct-previous-word-generic flyspell-mode-map))))

;; We only need this because of guess-language-lite. It would be nice if we
;; "installed" the packages from 'modules' in some sane way so that the
;; dependencies and autoloads would be properly handled.
(use-package guess-language
  :defer t
  :ensure t)

(use-package guess-language-lite
  :commands (gll-guess-language-lite-mode)
  :config
  (setq guess-language-languages '(en fi))
  (add-hook 'gll-language-identified-functions #'adq/guess-language-identified-hook)
  (when (adq/programs-p "enchant")
    (add-hook 'gll-language-identified-functions #'adq/guess-language-wcheck-hook)))

(defun adq/guess-language-identified-hook (lang)
  (pcase lang
    ('en (progn
           (setq-local typo-language "English") ; Maybe we should load typo before
           (writegood-mode)
           (flyspell-mode)))
    ('fi (progn
           (setq-local typo-language "Finnish")
           (when (adq/programs-p "enchant")
             (wcheck-mode))))))

(defvar adq/guess-language-code-to-wcheck-name-map
  '((fi . "Finnish")
    (en . "American English")))

;; Rename to wcheck hook and only add if available
(defun adq/guess-language-wcheck-hook (lang)
  (let ((current-code (car (rassoc 'wcheck-language
                                   adq/guess-language-code-to-wcheck-name-map))))
    (unless (eq current-code lang)
      (let ((lang-name (cdr (assoc lang
                                   adq/guess-language-code-to-wcheck-name-map))))
        (wcheck-change-language lang-name)))))


(provide 'init-check)

;;; init-check.el ends here
