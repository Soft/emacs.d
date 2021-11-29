;;; config-repl.el -*- lexical-binding: t; -*-

(defun adq/comint-clear-buffer ()
  "Clear current comint buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(use-package comint
  :straight nil
  :bind
  (:map comint-mode-map
        ("C-l" . adq/comint-clear-buffer)
        ("C-d" . quit-window))
  :config
  (add-hook
   'comint-exec-hook
   (lambda ()
     (set-process-query-on-exit-flag
      (get-buffer-process (current-buffer)) nil)))
  (setq comint-input-ring-size 10000))

(provide 'config-repl)
