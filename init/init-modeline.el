;;; init-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Use telephone-line for pretty modeline

;;; Code:

(use-package telephone-line-config
  :ensure telephone-line
  :init
  ;; Alternative flycheck segment that takes less space, uses all-the-icons and
  ;; looks better with different themes.
  (telephone-line-defsegment adq/telephone-line-flycheck-segment ()
    (when (bound-and-true-p flycheck-mode)
      (let ((text (pcase flycheck-last-status-change
                    ('finished (if flycheck-current-errors
                                   (let-alist (flycheck-count-errors flycheck-current-errors)
                                     (if (or .error .warning)
                                         (format "%s %s:%s"
                                                 (adq/icon-string 'octicon "alert")
                                                 (or .error 0)
                                                 (or .warning 0))
                                       ""))
                                 (adq/icon-string 'octicon "check")))
                    ('running (adq/icon-string 'faicon "spinner"))
                    ('no-checker (adq/icon-string 'octicon "dash"))
                    ('not-checked (adq/icon-string 'octicon "circle-slash"))
                    ('errored (adq/icon-string 'octicon "flame"))
                    ('interrupted (adq/icon-string 'octicon "stop"))
                    ('suspicious (adq/icon-string 'octicon "question")))))
        (propertize text
                    'help-echo (pcase flycheck-last-status-change
                                 ('finished "Display errors found by Flycheck")
                                 ('running "Running...")
                                 ('no-checker "No checker available")
                                 ('not-checked "Not checked")
                                 ('errored "Checker error")
                                 ('interrupted "Checker interrupted")
                                 ('suspicious "Checker in suspicious state"))
                    'display '(raise 0.0)
                    'mouse-face '(:box 1)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 #'flycheck-list-errors)))))
  (setq telephone-line-rhs
        '((nil    . (adq/telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

  (telephone-line-mode)
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
        telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
        telephone-line-primary-right-separator 'telephone-line-tan-right
        telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right
        telephone-line-evil-use-short-tag t))

(provide 'init-modeline)

;;; init-modeline.el ends here
