;;; config-fonts.el -*- lexical-binding: t; -*-

(defcustom adq/monospace-font-family
  "Iosevka"
  "Default monospace font."
  :type 'string
  :group 'adequate)

(defcustom adq/comment-font-family
  "Victor Mono"
  "Default comment font."
  :type 'string
  :group 'adequate)

(defcustom adq/term-font-family
  "Iosevka Term"
  "Default terminal font."
  :type 'string
  :group 'adequate)

(defcustom adq/variable-width-font-family
  "Iosevka Aile"
  "Default monospace font."
  :type 'string
  :group 'adequate)

(defcustom adq/font-size 11
  "Default font size."
  :type 'integer
  :group 'adequate)

(defvar adq/font-customizations
  `((font-lock-keyword-face
     . (:slant
        italic))
    (font-lock-comment-face
     . (:slant
        italic
        :weight
        normal
        :family
        ,adq/comment-font-family))
    (font-lock-doc-face
     . (:slant
        italic
        :weight
        normal
        :family
        ,adq/comment-font-family)))
  "Font customizations.")

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%d"
                               adq/monospace-font-family
                               adq/font-size)))

(set-face-attribute 'variable-pitch nil
                    :family adq/variable-width-font-family)

(use-package face-remap
  :bind
  (("<C-mouse-4>" . text-scale-increase)
   ("<C-mouse-5>" . text-scale-decrease)))

(defun adq/apply-font-customizations ()
  "Apply font customizations."
  (map-do
   (lambda (face properties)
     (apply #'set-face-attribute
            (append (list face nil) properties)))
   adq/font-customizations))

;; TODO: Think of a better way to connect these.
(add-hook 'adq/switch-theme-hook #'adq/apply-font-customizations)

(provide 'config-fonts)
