;;; init-xwidget.el --- Xwidgets -*- lexical-binding: t -*-

;;; Commentary:

;; Experimental configuration for xwidgets

;;; Code:

(defun adq/xwidget-webkit-setup ()
  "Defaults for Webkit"
  (bind-keys
   :map xwidget-webkit-mode-map
   ("<mouse-5>" . xwidget-webkit-scroll-up)
   ("<mouse-4>" . xwidget-webkit-scroll-down)
   ("<up>" . xwidget-webkit-scroll-up)
   ("<down>" . xwidget-webkit-scroll-down))
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (equal major-mode 'xwidget-webkit-mode)
                (xwidget-webkit-adjust-size-dispatch)))))

(add-hook 'xwidget-webkit-mode-hook 'xwidget-webkit-mode-defaults)

(provide 'init-xwidget)

;;; init-xwidget.el ends here
