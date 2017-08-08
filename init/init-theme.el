
(defun switch-theme (theme)
  "Unload existing theme and change to a new one."
  (interactive
   (list (intern
          (completing-read "Switch to theme: "
                           (mapcar #'symbol-name (custom-available-themes))
                           nil t))))
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))

(use-package subatomic-theme
  :ensure t
  :config (switch-theme 'subatomic))

(provide 'init-theme)