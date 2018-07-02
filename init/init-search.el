;;; init-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Search configuration. Additionally, are some search related things in
;; `init-helm.el'.

;;; Code:

(use-package pcre2el
  :ensure t
  :diminish pcre-mode
  :init (pcre-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode)
  (adq/after-load 'evil
    (use-package evil-anzu
      :ensure t
      :init (require 'evil-anzu))))

;; FIXME: This really needs more work
(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode ; Should this be hidden?
  )

(defvar adq/ctags-executable "ctags"
  "ctags executable name")

(defun adq/update-project-ctags ()
  "Update tags for the current project."
  (interactive)
  (let ((root (or (adq/git-find-repository-root)
                  (when buffer-file-name
                    (f-dirname buffer-file-name))))
        (start-time (current-time)))
    (if (and root (adq/programs-p adq/ctags-executable))
        (progn
          (message "Updating tags for %s"
                   (propertize root 'face font-lock-string-face))
          (async-start-process (format "%s: %s" adq/ctags-executable root)
                               adq/ctags-executable
                               (lambda (_)
                                 (let* ((end-time (current-time))
                                        (elapsed (time-subtract end-time start-time)))
                                   (message "Tags updated for %s in %s seconds"
                                            (propertize root 'face font-lock-string-face)
                                            (propertize (format "%.2f" (time-to-seconds elapsed))
                                                        'face
                                                        font-lock-constant-face))))
                               "-V"
                               "-R"
                               "-f"
                               (f-join root "TAGS")
                               root))
      (error "Could not update tags"))))

(use-package dumb-jump
  :ensure t
  ;;:config (setq dumb-jump-selector 'helm)
  :bind (("C-c d d" . dumb-jump-go)
         ("C-c d b" . dumb-jump-back)
         ("C-c d q" . dumb-jump-quick-look)))

(use-package ag
  :if (adq/programs-p "ag")
  :defer t
  :ensure t
  :config
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-reuse-buffers t))

(use-package ripgrep
  :if (adq/programs-p "rg")
  :defer t
  :ensure t)

(provide 'init-search)

;;; init-search.el ends here
