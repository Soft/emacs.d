;;; init-folding.el --- Folding -*- lexical-binding: t -*-

(use-package origami
  :ensure t
  :defer t
  :config
  (setq origami-show-fold-header t)
  (set-face-attribute
   'origami-fold-header-face nil
   :box nil
   :background 'unspecified))

(defhydra hydra-origami nil
  "
^Fold^               ^Control^    ^Move
^^^^^^-------------------------------------------
_<tab>_: Toggle      _u_: Undo    _n_: Next
    _o_: Open        _r_: Redo    _p_: Previous
    _O_: Open all
    _c_: Close
    _C_: Close all
    _1_: Show only
"
  ("<tab>" origami-recursively-toggle-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("u" origami-undo)
  ("r" origami-redo)
  ("o" origami-open-node)
  ("O" origami-open-all-nodes)
  ("c" origami-close-node)
  ("C" origami-close-all-nodes)
  ("1" origami-show-only-node))

(bind-key "C-c f" #'hydra-origami/body)

(provide 'init-folding)
