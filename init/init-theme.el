;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Theme related settings

;; I like themes
(defvar wanted-themes
  '(atom-one-dark-theme
    challenger-deep-theme
    django-theme
    doom-themes
    dracula-theme
    espresso-theme
    gotham-theme
    hemisu-theme
    material-theme
    monokai-theme
    plan9-theme
    spacemacs-theme
    subatomic-theme
    sublime-themes
    twilight-anti-bright-theme
    twilight-bright-theme)
  "Themes that will be automatically installed if they are missing.")

(install-packages-if-missing wanted-themes t)

(defun switch-theme (theme)
  "Unload existing theme and switch to a new one."
  (interactive
   (list (intern
          (completing-read "Switch to theme: "
                           (mapcar #'symbol-name (custom-available-themes))
                           nil t))))
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))

(defun current-theme ()
  "Displays the current theme."
  (interactive)
  (message "%s" (car custom-enabled-themes)))

(defvar favorite-themes
  '(subatomic gotham material-light atom-one-dark spacemacs-light)
  "List of favorite themes for use with rand-theme.")

(defun rand-theme ()
  "Switch to a random theme from favorite-themes."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (themes (--remove (eq it current) favorite-themes))
         (selected (list-random-item themes)))
    (when selected
      (switch-theme selected))))

(use-package color-theme-approximate
  :if (eq system-type 'gnu/linux)
  :ensure t
  :init (color-theme-approximate-on))

(use-package gtk-style-ext
  :if (and (featurep 'gtk) (locate-library "gtk-style-ext-sys"))
  :diminish gtk-style-ext-adapt-to-theme-mode
  :init
  (gtk-style-ext-dark-theme-mode 1)
  (gtk-style-ext-adapt-to-theme-mode 1))

(switch-theme 'doom-vibrant)

(provide 'init-theme)
