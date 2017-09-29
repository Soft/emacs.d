;;; init-folding.el --- Folding -*- lexical-binding: t -*-

(use-package origami
  :ensure t
  :defer t
  :bind
  (("C-c f o" . origami-open-node)
   ("C-c f O" . origami-open-all-nodes)
   ("C-c f c" . origami-close-node)
   ("C-c f C" . origami-close-all-nodes)
   ("C-c f 1" . origami-show-only-node)))

(defhydra hydra-origami-toggle (global-map "C-c f")
  "Recursively toggle nodes."
  ("<tab>" #'origami-recursively-toggle-node)
  ("n" #'origami-next-fold)
  ("p" #'origami-previous-fold)
  ("u" #'origami-undo)
  ("r" #'origami-redo))

(provide 'init-folding)
