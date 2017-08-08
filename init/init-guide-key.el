
(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence
          '("C-c p" "C-x 4" "C-x 5" "C-x c" "C-c !"
            "C-x r" "C-x a" "C-c n" "C-c w" "C-c o"
            "C-x x" "C-x 8")
          guide-key/recursive-key-sequence-flag t
          guide-key/idle-delay 0.1
          guide-key/popup-window-position 'bottom)
    (guide-key-mode)))

(provide 'init-guide-key)
