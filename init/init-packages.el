;;; init-packages.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Use Paradox for pretty package menu. Additionally, we try to make sure we
;; have a fresh copy package archive data before trying to install packages.

;;; Code:

(use-package paradox
  :ensure t
  :defer t
  :commands (paradox-enable)
  :config
  (advice-add ;; Enable Paradox silently
   'paradox--override-definition :around
   (lambda (fn &rest args)
     (cl-letf (((symbol-function 'message) (lambda (&rest args))))
       (apply fn args)))))

(use-package package-utils
  :defer t
  :ensure t)

(add-hook 'after-init-hook #'paradox-enable)

(defvar adq/package-archive-old-seconds (* 120 60)
  "When should package archive data be considered old.")

(defun adq/package-should-refresh-p ()
  "Should package archive be refreshed."
  (if adq/package-last-refresh-time
      (if  (time-less-p
            (time-subtract (current-time) adq/package-last-refresh-time)
            (seconds-to-time adq/package-archive-old-seconds))
          nil
        t) 
    t))

(defun adq/install-packages-if-missing (packages &optional refresh)
  "Install PACKAGES if they are not already installed. If REFRESH
is non-nil, refresh packages before installing if
`adq/package-should-refresh-p' returns non-nil."
  (let ((to-install (-remove #'package-installed-p packages)))
    (when to-install
      (when (and refresh (adq/package-should-refresh-p))
        (package-refresh-contents))
      (-each to-install #'package-install))))

;; This might not be that useful since use-package seems to refresh the package
;; archive
(defun adq/use-package-refresh-if-required (name args state &optional no-refresh)
  "Modify use-package's :ensure to refresh package archive when
required."
  (let ((package (use-package-as-symbol
                  (if (eq (car args) t)
                      name (car args)))))
    (when (not (package-installed-p package))
      (message "%s" package)
      (when (adq/package-should-refresh-p)
        (package-refresh-contents)))))

(advice-add #'use-package-ensure-elpa :before #'adq/use-package-refresh-if-required)


(provide 'init-packages)

;;; init-packages.el ends here
