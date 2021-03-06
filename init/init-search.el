;;; init-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Search configuration. There are some additional search related options in
;; `init-helm.el'.

;;; Code:

(use-package pcre2el
  :disabled ;; This seems to cause some errors.
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

(use-package xref
  :bind (:map xref--xref-buffer-mode-map
              ("q" . quit-window)
              ("<escape>" . quit-window)
              ("j" . xref-next-line)
              ("k" . xref-prev-line)
              ("J" . evil-scroll-down)
              ("K" . evil-scroll-up)
              ("<return>" . xref-goto-xref)
              ("S-<return>" . xref-show-location-at-point)))

(use-package replace
  :bind (:map occur-mode-map
              ("j" . occur-next)
              ("k" . occur-prev)
              ("<escape>" . quit-window)))

(setq tags-revert-without-query t)

(defvar adq/ctags-executable "ctags"
  "ctags executable name")

(defun adq/show-tags-table-list ()
  "Show tags table list."
  (interactive)
  (message (s-join
            (propertize "; " 'face font-lock-constant-face)
            (or tags-table-list
                (list tags-file-name)))))

(defun adq/current-project-root ()
  "Get path to the current project root. This is the root of the
git repository or, if no git repository is present, directory
containing the current file. Function returns nil if the current
buffer is not associated with a file."
  (or (adq/git-find-repository-root)
      (when buffer-file-name
        (f-dirname buffer-file-name))))

(defun adq/setup-project-ctags ()
  "Create tags for current project if they do not exist and set
the tags file as active."
  (interactive)
  (let ((root (adq/current-project-root)))
    (if root
        (let ((tags-file (f-join root "TAGS")))
          (if (f-exists-p tags-file)
              (add-to-list 'tags-table-list tags-file)
            (adq/update-project-ctags t)))
      (error "Not in a project"))))

(defun adq/update-project-ctags (&optional visit)
  "Update tags for the current project. If the optional VISIT is
supplied the tags file is visited once it has been generated."
  (interactive)
  (let ((root (adq/current-project-root))
        (start-time (current-time)))
    (if (and root (adq/programs-p adq/ctags-executable))
        (let ((tags-file (f-join root "TAGS")))
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
                                                        font-lock-constant-face))
                                   (when visit
                                     (add-to-list 'tags-table-list tags-file))))
                               "-V"
                               "-e"
                               "-R"
                               "-f"
                               tags-file
                               root))
      (error "Could not update tags"))))

(defun adq/disable-tags ()
  "Disable all tags."
  (interactive)
  (message "Disabling all tags")
  (setq tags-table-list nil
        tags-file-name nil))

(use-package etags
  :defer t
  :commands (tags-reset-tags-tables))

(bind-keys
 :prefix-map adq/tags-prefix-keymap
 :prefix-docstring "Keymap for operating with tags."
 :prefix "C-c d"
 ("U" . adq/setup-project-ctags)
 ("u" . adq/update-project-ctags)
 ("c" . adq/disable-tags)
 ("a" . xref-find-apropos)
 ("d" . xref-find-definitions)
 ("D" . xref-find-definitions-other-window)
 ("r" . xref-find-references))

(use-package ag
  :if (adq/programs-p "ag")
  :defer t
  :ensure t
  :config
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-reuse-buffers t))

(use-package deadgrep
  :if (adq/programs-p "rg")
  :ensure t
  :bind (("C-c x s" . deadgrep)
         :map deadgrep-mode-map
         ("j" . deadgrep-forward)
         ("k" . deadgrep-backward)
         ("J" . evil-scroll-down)
         ("K" . evil-scroll-up)
         ("<escape>" . quit-window)))

(provide 'init-search)

;;; init-search.el ends here
