;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Make dired nice.

;;; Code:

(defun adq/dired-setup ()
  "Setup dired-mode."
  (diredfl-mode)
  (adq/if-supported all-the-icons-dired-mode))

;; TODO: Show prompt when universal argument is supplied.
(defun adq/dired-get-buffer (d)
  "Open dired with default directory or, if universal argument
was supplied, with project root directory."
  (interactive "P")
  (dired (or (and d (projectile-project-root)) default-directory)))

(use-package diredfl
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :if (featurep 'all-the-icons)
  :ensure t
  :defer t)

(use-package dired-single
  :ensure t
  :defer t)

(use-package dired-launch
  :ensure t
  :defer t)

(use-package dired-quick-sort
  :ensure t
  :defer t
  :commands  (hydra-dired-quick-sort/body))

(use-package dired-filter
  :ensure t
  :defer t)

(use-package dired
  :bind (("C-x d" . adq/dired-get-buffer))
  :config
  (add-hook 'dired-mode-hook #'adq/dired-setup)
  (defhydra adq/hydra-dired-filter nil
    "
^Filter by^       ^Compose^           ^Control
^^^^^^----------------------------------------------------
_n_: Name             _|_: Or         _p_: Pop
_r_: Regexp           _!_: Negate     _/_: Pop all
_._: Extension        _*_: Decompose  _S_: Save
_h_: Hidden       _<tab>_: Transpose  _D_: Delete saved
_o_: Omit         ^     ^             _A_: Add saved
_g_: Garbage      ^     ^             _L_: Load saved
_e_: Predicate
_f_: File
_d_: Directory
_m_: Mode
_s_: Symlink
_x_: Executable
"
    ("n" dired-filter-by-name)
    ("r" dired-filter-by-regexp)
    ("." dired-filter-by-extension)
    ("h" dired-filter-by-dot-files)
    ("o" dired-filter-by-omit)
    ("g" dired-filter-by-garbage)
    ("e" dired-filter-by-predicate)
    ("f" dired-filter-by-file)
    ("d" dired-filter-by-directory)
    ("m" dired-filter-by-mode)
    ("s" dired-filter-by-symlink)
    ("x" dired-filter-by-executable)
    ("|" dired-filter-or)
    ("!" dired-filter-negate)
    ("*" dired-filter-decompose)
    ("<tab>" dired-filter-transpose)
    ("p" dired-filter-pop)
    ("/" dired-filter-pop-all)
    ("S" dired-filter-save-filters)
    ("D" dired-filter-delete-saved-filters)
    ("A" dired-filter-add-saved-filters)
    ("L" dired-filter-load-saved-filters))
  (bind-keys
   :map dired-mode-map
   ("<return>" . dired-single-buffer)
   ("<mouse-1>" . dired-single-buffer-mouse)
   ("S" . hydra-dired-quick-sort/body)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("<tab>" . dired-next-line)
   ("<backtab>" . dired-previous-line)
   ("f" . adq/hydra-dired-filter/body)))

(provide 'init-dired)

;;; init-dired.el ends here
