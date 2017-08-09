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

(defun select-font (&rest fonts)
  (if (daemonp) ; For some reason find-font doesn't seem to like daemon mode
      (car fonts)
    (or
     (--first (find-font (font-spec :name it)) fonts)
     (find-font (font-spec :name "monospace")))))

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%d"
                               (apply #'select-font monospace-font-family)
                               font-size)))

(add-hook 'text-mode-hook
          (lambda ()
            (setq-local buffer-face-mode-face
                        `(:family ,(apply #'select-font variable-font-family)))
            (buffer-face-mode)))

(bind-keys ("<C-mouse-4>" . text-scale-increase)
           ("<C-mouse-5>" . text-scale-decrease))

(provide 'init-fonts)
