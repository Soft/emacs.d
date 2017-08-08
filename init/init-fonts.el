
(defvar font-family
  '("Iosevka"
    "Hack"
    "Fira Mono"
    "Droid Sans Mono"
    "Source Code Pro"
    "Dejavu Sans Mono"))

(defvar font-size
  (if (or (>= (display-pixel-width) 1680) (daemonp)) 11 8))

(defun select-font (&rest fonts)
  (or
   (--first (find-font (font-spec :name it)) fonts)
   (find-font (font-spec :name "monospace"))))

(add-to-list 'default-frame-alist
  `(font . ,(format "%s-%d" (apply 'select-font font-family) font-size)))

(bind-keys ("<C-mouse-4>" . text-scale-increase)
           ("<C-mouse-5>" . text-scale-decrease))

(provide 'init-fonts)
