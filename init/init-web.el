;;; init-web.el --- Web Browsing -*- lexical-binding: t -*-

;;; Commentary:

;; Web Browser configuration

;;; Code:

(use-package eww
  :bind (:map eww-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("h" . left-char)
              ("l" . right-char)
              ("J" . scroll-up)
              ("K" . scroll-down)
              ("o" . eww)
              ("O" . eww-browse-with-external-browser)
              ("<return>" . eww-follow-link)
              ("[" . eww-previous-url)
              ("]" . eww-next-url)
              ("g" . eww-reload)
              ("H" . eww-back-url)
              ("L" . eww-forward-url)
              ("<backspace>" . eww-back-url)
              ("<left>" . eww-back-url)
              ("<right>" . eww-forward-url)
              ("<tab>" . shr-next-link)
              ("<backtab>" . shr-previous-link)
              ("B" . eww-list-bookmarks)
              ("I" . eww-list-histories)
              ("b" . eww-list-buffers)
              ("q" . quit-window))
  :init
  (autoload 'eww-read-bookmarks "eww")
  (adq/after-load 'dashboard
    (defun adq/dashboard-insert-eww-bookmarks-list (list-display-name list)
      (when (car list)
        (dashboard-insert-heading list-display-name)
        (mapc (lambda (bookmark)
                (insert "\n    ")
                (widget-create 'push-button
                               :action (lambda (&rest ignore)
                                         (eww (plist-get bookmark :url)))
                               :mouse-face 'highlight
                               :follow-link "\C-m"
                               :button-prefix ""
                               :button-suffix ""
                               :format "%[%t%]"
                               (format
                                "%s - %s"
                                (plist-get bookmark :title)
                                (plist-get bookmark :url))))
              list)))

    (defun adq/dashboard-insert-eww-bookmarks (list-size)
      (when (adq/dashboard-insert-eww-bookmarks-list
             "eww:"
             (dashboard-subseq (eww-read-bookmarks) 0 list-size))
        (dashboard-insert-shortcut "E" "eww:")))

    (add-to-list 'dashboard-item-generators '(eww . adq/dashboard-insert-eww-bookmarks))))

(provide 'init-web)

;;; init-web.el ends here
