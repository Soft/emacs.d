;;; init-folding.el --- Folding -*- lexical-binding: t -*-

(use-package origami
  :ensure t
  :defer t)

(defhydra hydra-origami (global-map "C-c f")
  "Folding"
  ("<tab>" origami-recursively-toggle-node "toggle")
  ("n" origami-next-fold "next")
  ("p" origami-previous-fold "previous")
  ("u" origami-undo "undo")
  ("r" origami-redo "redo")
  ("o" origami-open-node "open node")
  ("O" origami-open-all-nodes "open all nodes")
  ("c" origami-close-node "close node")
  ("C" origami-close-all-nodes "close all nodes")
  ("1" origami-show-only-node "show only node"))

(provide 'init-folding)
