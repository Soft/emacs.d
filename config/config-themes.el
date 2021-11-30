;;; config-themes.el -*- lexical-binding: t; -*-

(use-package kaolin-themes :defer t
  :config
  (setq kaolin-themes-underline-wave nil))

(use-package doom-themes :defer t)

(use-package modus-themes :defer t)

(defvar adq/favorite-themes
  '(kaolin-galaxy
    kaolin-dark
    kaolin-bubblegum
    kaolin-aurora
    kaolin-ocean
    doom-one
    doom-monokai-pro
    doom-dracula
    doom-xcode
    modus-vivendi))

(load-theme
 (seq-random-elt adq/favorite-themes) t)

(defvar adq/switch-theme-hook nil
  "Hook to be run after switch theme.")

(defun adq/switch-theme (theme)
  "Unload existing theme and switch to a new one."
  (interactive
   (list (intern
          (completing-read "Switch to theme: "
                           (mapcar #'symbol-name (custom-available-themes))
                           nil t))))
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t)
  (run-hooks 'adq/switch-theme-hook))

(defun adq/current-theme ()
  "Displays the current theme."
  (interactive)
  (message "%s" (car custom-enabled-themes)))
  
(provide 'config-themes)
