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
  (set-face-attribute
   'bm-fringe-face nil
   :foreground "#ff14cc"
   :background nil)
  (set-face-attribute
   'bm-fringe-persistent-face nil
   :foreground "#ff1452"
   :background nil)
  (bind-keys
   :map bm-show-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  (advice-add 'bm-bookmark-add :after (lambda (&rest args) (bm-save)))
  (advice-add 'bm-bookmark-remove :after (lambda (&rest args) (bm-save)))
  (setq bm-cycle-all-buffers t
        bm-highlight-style 'bm-highlight-only-fringe
        bm-repository-size 1000)
  (setq-default bm-buffer-persistence t)
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
    "Generate a list of all bookmarks, including bookmerks from
files that are not currently open."
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
