;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

(defvar monospace-font-family
  '("Iosevka"
    "Hack"
    "Fira Mono"
    "Droid Sans Mono"
    "Source Code Pro"
    "Dejavu Sans Mono")
  "List of monospace fonts to use")

(defvar variable-font-family
  '("Fira Sans"
    "Noto Sans"
    "Dejavu Sans")
  "List of variable-width fonts to use")

(defvar font-size
  (if (or (>= (display-pixel-width) 1680)
          (daemonp))
      11 8))

(defun select-font (fonts fallback)
  (or
   (--first (find-font (font-spec :name it)) fonts)
   fallback))

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%d"
                               (select-font monospace-font-family "Fira Mono")
                               font-size)))

(add-hook 'text-mode-hook
          (lambda ()
            (setq-local buffer-face-mode-face
                        `(:family ,(select-font variable-font-family "Fira Sans")))
            (buffer-face-mode)))

(bind-keys ("<C-mouse-4>" . text-scale-increase)
           ("<C-mouse-5>" . text-scale-decrease))

(provide 'init-fonts)
