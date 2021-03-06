;;; init-bookmarks.el --- Bookmarks -*- lexical-binding: t -*-

;;; Commentary:

;; Use bm for bookmarks.

;;; Code:

(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t
        bm-repository-file (f-join user-emacs-directory "bm-data"))
  :config
  (fringe-helper-define
    'bm-marker-left nil
    "......."
    ".XX.XX."
    "XXXXXXX"
    "XXXXXXX"
    ".XXXXX."
    "..XXX.."
    "...X..."
    ".......")

  (defface adq/bm-fringe-face
    '((t :foreground "#ff14cc"))
    "Face for bookmarks.")
  (defface adq/bm-fringe-persistent-face
    '((t :foreground "#ff1452"))
    "Face for persistent bookmarks.")
  (setq bm-fringe-face 'adq/bm-fringe-face
        bm-fringe-persistent-face 'adq/bm-fringe-persistent-face)

  (bind-keys
   :map bm-show-mode-map
   ("j" . next-line)
   ("k" . previous-line))

  (setq bm-cycle-all-buffers t
        bm-highlight-style 'bm-highlight-only-fringe
        bm-repository-size 1000)
  (setq-default bm-buffer-persistence t)

  (defun adq/bm-save ()
    "Save bookmarks to persistent repository."
    (interactive)
    (bm-buffer-save-all)
    (bm-repository-save))

  (advice-add 'bm-bookmark-add
              :after (lambda (&rest args)
                       (adq/bm-save)))
  (advice-add 'bm-bookmark-remove
              :after (lambda (&rest args)
                       (adq/bm-save)))
  (add-hook 'after-init-hook #'bm-repository-load)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-rever-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (bm-buffer-save-all)
              (bm-repository-save)))

  (defun adq/bm-list-all-bookmarks ()
    "Generate a list of all bookmarks from all files."
    (let ((bookmarks '()))
      (cl-loop for entry in bm-repository
               when (and (listp entry) (f-readable-p (car entry)))
               do
               (with-temp-buffer
                 (insert-file-contents-literally (car entry))
                 (cl-letf (((symbol-function 'bm-bookmark-add)
                            (lambda (&optional annotation time temporary)
                              (!cons (list (car entry)
                                           (point)
                                           (line-number-at-pos)
                                           (string-trim (thing-at-point 'line t)))
                                     bookmarks)))
                           ((symbol-function 'message)
                            (lambda (&rest _))))
                   (bm-buffer-restore-2 (cdr entry)))))
      bookmarks))

  (adq/after-load 'helm
    (require 'compile)

    (defun adq/helm-bm-all-format-bookmark (bookmark)
      "Format bookmark for display."
      (let ((file (f-filename (car bookmark)))
            (line (caddr bookmark))
            (contents (cadddr bookmark)))
        (cons
         (format "%s:%s: %s"
                 (propertize file 'face compilation-info-face)
                 (propertize (format "%d" line) 'face compilation-line-face)
                 contents)
         bookmark)))

    (defvar adq/helm-bm-all-source
      (helm-build-sync-source "Helm All Bookmarks"
        :candidates
        (lambda ()
          (mapcar #'adq/helm-bm-all-format-bookmark
                  (adq/bm-list-all-bookmarks)))
        :action
        '(("Switch to buffer" .
           (lambda (bookmark)
             (find-file (car bookmark))
             (goto-char (cadr bookmark))))))
      "Helm source with all the bookmarks.")

    (defun adq/helm-bm-list-all ()
      "List all bookmarks usin Helm."
      (interactive)
      (helm :sources 'adq/helm-bm-all-source
            :buffer "*helm bm all*")))

  (adq/after-load 'dashboard
    (defun adq/dashboard-insert-bm-list (list-display-name list)
      (when (car list)
        (dashboard-insert-heading list-display-name)
        (mapc (lambda (bookmark)
                (insert "\n    ")
                (widget-create 'push-button
                               :action (lambda (&rest ignore)
                                         (find-file (car bookmark))
                                         (goto-char (cadr bookmark)))
                               :mouse-face 'highlight
                               :follow-link "\C-m"
                               :button-prefix ""
                               :button-suffix ""
                               :format "%[%t%]"
                               (format "%s:%d: %s"
                                       (f-filename (car bookmark))
                                       (caddr bookmark)
                                       (cadddr bookmark))))
              list)))

    (defun adq/dashboard-insert-bm (list-size)
      (when (adq/dashboard-insert-bm-list
             "Bookmarks:"
             (dashboard-subseq (adq/bm-list-all-bookmarks) 0 list-size))
        (dashboard-insert-shortcut "B" "Bookmarks:")))

    (add-to-list 'dashboard-item-generators '(bm . adq/dashboard-insert-bm)))

  (defhydra adq/hydra-bm nil
    "
Bookmarks
^^^^------------------------------------------------
_m_: Toggle      _l_: Bookmarks from Buffers
_n_: Next        _a_: Bookmarks form All Files
_p_: Previous    _L_: List
"
    ("m" bm-toggle)
    ("n" bm-next)
    ("p" bm-previous)
    ("a" adq/helm-bm-list-all :exit t)
    ("l" helm-bm :exit t)
    ("L" bm-show-all :exit t))
  (bind-key "C-c m" #'adq/hydra-bm/body))

(provide 'init-bookmarks)

;;; init-bookmarks.el ends here
