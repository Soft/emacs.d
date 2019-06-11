;;; init-feed.el --- Feed Aggregator -*- lexical-binding: t -*-

;;; Commentary:

;; Feed aggregator configuration

;;; Code:

(use-package elfeed
  :ensure t
  :defer t
  :bind
  (("C-c x F" . elfeed)
   :map elfeed-search-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("K" . scroll-down)
   ("J" . scroll-up)
   :map elfeed-show-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("K" . scroll-down)
   ("J" . scroll-up)))

(provide 'init-feed)

;;; init-feed.el ends here
