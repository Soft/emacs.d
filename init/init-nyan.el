
(use-package nyan-mode
  :ensure t)

;; It's Friday!
(when (eq (string-to-int (format-time-string "%u")) 5)
  (nyan-mode)
  (nyan-start-animation)
  (nyan-toggle-wavy-trail))

(provide 'init-nyan)
