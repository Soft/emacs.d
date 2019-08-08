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

(defun adq/elfeed-toggle-images ()
  "Toggle image display."
  (interactive)
  (if shr-inhibit-images
      (progn
        (setq-local shr-inhibit-images nil)
        (elfeed-show-refresh))
    (progn
      (setq-local shr-inhibit-images t)
      (elfeed-show-refresh))))

(defun adq/elfeed-toggle-unread ()
  "Toggle unread tag for selected entries."
  (interactive)
  (elfeed-search-toggle-all 'unread))

(use-package elfeed
  :ensure t
  :commands
  (elfeed-search-toggle-all
   elfeed-show-refresh)
  :bind
  (("C-c x F" . elfeed)
   :map elfeed-search-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("K" . scroll-down)
   ("J" . scroll-up)
   ("m" . adq/elfeed-toggle-unread)
   ("o" . elfeed-search-show-entry)
   ("/" . elfeed-search-set-filter)
   :map elfeed-show-mode-map
   ("f" . eww-follow-link)
   ("i" . adq/elfeed-toggle-images)
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("K" . scroll-down)
   ("J" . scroll-up)
   ("n" . (lambda ()
            (interactive)
            (let ((elfeed-show-entry-switch #'switch-to-buffer))
              (call-interactively #'elfeed-show-next))))
   ("p" . (lambda ()
            (interactive)
            (let ((elfeed-show-entry-switch #'switch-to-buffer))
              (call-interactively #'elfeed-show-prev)))))
  :config
  (add-hook 'elfeed-show-mode-hook #'adq/elfeed-show-setup)
  (setq elfeed-show-entry-switch #'switch-to-buffer-other-window))

(provide 'init-feed)

;;; init-feed.el ends here
