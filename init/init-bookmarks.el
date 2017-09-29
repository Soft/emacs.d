;;; init-bookmarks.el --- Bookmarks -*- lexical-binding: t -*-

(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t
        bm-repository-file (f-join user-emacs-directory "bm-data"))
  :config
  (fringe-helper-define
    'bm-marker-left 'center
    ".XX.XX."
    "X..X..X"
    "X.....X"
    ".X...X."
    "..X.X.."
    "...X...")
  (setq bm-cycle-all-buffers t
        bm-highlight-style 'bm-highlight-only-fringe
        bm-repository-size 1000
        bm-fringe-face '(:foreground "#ff14cc") ;; FIXME: These do not seem to work
        bm-fringe-persistent-face '(:foreground "#ff1452"))
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
  (defhydra bm-hydra (global-map "C-c m")
    "bookmark"
    ("m" bm-toggle "toggle")
    ("n" bm-next "next")
    ("p" bm-previous "previous")
    ("l" bm-show-all "list" :exit t)))

(provide 'init-bookmarks)
