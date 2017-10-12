;;; init-space.el --- Spacemacs inspired keymaps -*- lexical-binding: t -*-

;;; Commentary:

;; Some keybindings inspired by Spacemacs.

;;; Code:

(adq/after-load 'evil
  (bind-keys
   :map evil-normal-state-map
   ("SPC SPC" . helm-M-x)

   ;; Search
   ("SPC s s" . isearch-forward)
   ("SPC s S" . helm-swoop)
   ("SPC s r" . isearch-forward-regexp)
   ("SPC s o" . helm-occur)
   ("SPC s g" . helm-projectile-ag)

   ;; Replace
   ("SPC r r" . query-replace)
   ("SPC r R" . query-replace-regexp)

   ;; Folding
   ("SPC o c" . origami-show-only-node)

   ;; Buffer Operations
   ("SPC b f" . helm-find-files)
   ("SPC b b" . helm-mini)
   ("SPC b B" . helm-buffer-list)
   ("SPC b p" . helm-projectile-find-file)
   ("SPC b r" . helm-recentf)

   ;; File Operations   
   ("SPC f s" . save-buffer)
   ("SPC f h" . undo-tree-visualize)
   ("SPC f r" . revert-buffer)
   ("SPC f c" . cd)

   ;; Jump Commands
   ("SPC j i" . helm-imenu)
   ("SPC j I" . helm-imenu-in-all-buffers)
   ("SPC j j" . avy-goto-char)
   ("SPC j J" . avy-goto-char-2)
   ("SPC j l" . avy-goto-line)
   ("SPC j w" . avy-goto-word-1)
   ("SPC j u" . browse-url-at-point)

   ;; Window Operations
   ("SPC w k" . adq/kill-window-and-maybe-buffer)
   ("SPC w t" . neotree-toggle)

   ;; Misc
   ("SPC x t" . adq/google-translate-with-defaults)
   ("SPC x l" . nlinum-mode)
   ("SPC x g" . highlight-indent-guides-mode)
   ("SPC x w" . whitespace-mode)
   ("SPC x T" . toggle-truncate-lines)
   ("SPC x b" . browse-url-at-point)
   ("SPC x X" . xkcd-rand)))

(defhydra adq/hydra-rotate-frame (evil-normal-state-map "SPC w")
  "Rotate frame"
  ("r" #'rotate-frame-clockwise "clockwise")
  ("R" #'rotate-frame-anticlockwise "counterclockwise"))

(defhydra adq/hydra-windmove (evil-normal-state-map "SPC w m")
  "Move between windows"
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right"))

(defhydra adq/hydra-zoom (evil-normal-state-map "SPC w")
  "Text size"
  ("+" text-scale-increase "increase")
  ("-" text-scale-decrease "decrease"))

(defhydra adq/hydra-winner (evil-normal-state-map "SPC w")
  "Window history"
  ("n" winner-redo "redo")
  ("p" winner-undo "undo"))

(provide 'init-space)

;;; init-space.el ends here
