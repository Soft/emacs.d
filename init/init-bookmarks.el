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
  (defhydra hydra-bm nil
    "
Bookmark
^^^^-------------------------
_m_: Toggle      _l_: List
_n_: Next
_p_: Previous
"
    ("m" bm-toggle)
    ("n" bm-next)
    ("p" bm-previous)
    ("l" bm-show-all :exit t))
  (bind-key "C-c m" #'hydra-bm/body))

(provide 'init-bookmarks)

;;; init-bookmarks.el ends here
