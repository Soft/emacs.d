;;; init-feed.el --- Feed Aggregator -*- lexical-binding: t -*-

;;; Commentary:

;; Feed aggregator configuration.

;; Set elfeed-feeds to a list of URLs for feeds.

;;; Code:


(defvar adq/elfeed-display-width 80
  "Content width for elfeed-show-mode.")


(defun adq/elfeed-show-setup ()
  "Setup elfeed-show-mode."
  (when (-all?
         (lambda (window)
           (> (window-body-width window)
              adq/elfeed-display-width))
         (get-buffer-window-list))
    (setq-local shr-width adq/elfeed-display-width))
  (setq-local shr-bullet "• "))


(use-package elfeed
  :ensure t
  :bind
  (("C-c x F" . elfeed)
   :map elfeed-search-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("K" . scroll-down)
   ("J" . scroll-up)
   ("o" . elfeed-show-entry)
   ("/" . elfeed-search-set-filter)
   :map elfeed-show-mode-map
   ("f" . eww-follow-link)
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("K" . scroll-down)
   ("J" . scroll-up))
  :config
  (add-hook 'elfeed-show-mode-hook #'adq/elfeed-show-setup))

(provide 'init-feed)

;;; init-feed.el ends here
