;;; init-theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Make Emacs look nice.

;;; Code:

;; I like themes
(defvar adq/wanted-themes
  '(atom-one-dark-theme
    borland-blue-theme
    challenger-deep-theme
    darkokai-theme
    django-theme
    doom-themes
    dracula-theme
    espresso-theme
    gotham-theme
    gruvbox-theme
    hemisu-theme
    kaolin-themes
    kooten-theme
    material-theme
    modus-operandi-theme
    modus-vivendi-theme
    monokai-theme
    plan9-theme
    spacemacs-theme
    subatomic-theme
    sublime-themes
    twilight-anti-bright-theme
    twilight-bright-theme
    zenburn-theme)
  "Themes that will be automatically installed if they are missing.")

(adq/install-packages-if-missing adq/wanted-themes t)

(defvar adq/default-theme 'doom-horizon
  "Default theme for Emacs.")

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

(defvar adq/favorite-themes
  '(subatomic gotham material-light atom-one-dark spacemacs-light)
  "List of favorite themes for use with rand-theme.")

(defun adq/random-theme ()
  "Switch to a random theme from adq/favorite-themes."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (themes (--remove (eq it current) adq/favorite-themes))
         (selected (adq/list-random-item themes)))
    (when selected
      (adq/switch-theme selected)
      (adq/current-theme))))

(use-package color-theme-approximate
  :if (eq system-type 'gnu/linux)
  :ensure t
  :init (color-theme-approximate-on))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (adq/switch-theme adq/default-theme)))
  (add-hook 'after-init-hook
            (lambda () (adq/switch-theme adq/default-theme))))


(provide 'init-theme)

;;; init-theme.el ends here
