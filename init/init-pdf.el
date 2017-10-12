;;; init-pdf.el --- Viewing PDF files -*- lexical-binding: t -*-

;;; Commentary:

;; PDF Viewer.

;;; Code:

(defun adq/pdf-view-setup ()
  (pdf-tools-enable-minor-modes)
  (pdf-view-fit-page-to-window)
  (auto-revert-mode 1))

(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (add-hook 'pdf-view-mode-hook #'adq/pdf-view-setup)
  (bind-keys
   :map pdf-view-mode-map
   ("j" . pdf-view-next-line-or-next-page)
   ("k" . pdf-view-previous-line-or-previous-page)
   ("J" . pdf-view-next-page)
   ("K" . pdf-view-previous-page)))

(provide 'init-pdf)

;;; init-pdf.el ends here
