;;; init-web.el --- Web Browsing -*- lexical-binding: t -*-

;;; Commentary:

;; Web Browser configuration

;;; Code:

(defun adq/eww-mode-setup ()
  "Defaults for eww-mode buffers."
  (setq-local shr-color-visible-luminance-min 70))

(use-package eww-lnum
  :ensure t
  :defer t)

(use-package eww
  :bind
  (("C-c x e" . eww)
   :map eww-mode-map
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
   ("<tab>" . shr-next-link)
   ("<backtab>" . shr-previous-link)
   ("B" . eww-list-bookmarks)
   ("I" . eww-list-histories)
   ("U" . eww-list-buffers)
   ("A" . eww-add-bookmark)
   ("c" . eww-toggle-colors)
   ("f" . eww-lnum-follow)
   ("F" . eww-toggle-fonts)
   ("q" . quit-window)
   :map eww-bookmark-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . scroll-up)
   ("K" . scroll-down)
   :map eww-history-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . scroll-up)
   ("K" . scroll-down))
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
             "Eww:"
             (dashboard-subseq (eww-read-bookmarks) 0 list-size))
        (dashboard-insert-shortcut "E" "Eww:")))

    (add-to-list 'dashboard-item-generators '(eww . adq/dashboard-insert-eww-bookmarks)))

  (adq/after-load 'helm
    (defun adq/helm-eww-bookmarks-candidates ()
      "Helm candidate source for Eww bookmarks."
      (mapcar (lambda (bookmark)
                (let ((title (plist-get bookmark :title))
                      (url (plist-get bookmark :url)))
                  (cons (format "%s - %s"
                                (propertize title 'face font-lock-type-face)
                                (propertize url 'face font-lock-string-face))
                        url)))
              (eww-read-bookmarks)))

    (defun adq/helm-eww-bookmarks ()
      "List Eww bookmarks using Helm."
      (interactive)
      (helm :sources (helm-build-sync-source "eww-bookmarks"
                       :candidates #'adq/helm-eww-bookmarks-candidates
                       :action (helm-make-actions
                                "Browse" #'eww))
            :buffer "*helm eww-bookmarks*")))
  (add-hook 'eww-mode-hook #'adq/eww-mode-setup))

(provide 'init-web)

;;; init-web.el ends here
