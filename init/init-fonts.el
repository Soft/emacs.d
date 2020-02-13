;;; init-fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Font configuration.

;; Adequate has a CSS-style font fallback system where upon start it
;; successively tries a series of recommended fonts until it finds one that is
;; available.

;; Currently, Adequate tries to first use Iosevka font
;; <https://be5invis.github.io/Iosevka/>.

;;; Code:

(defvar adq/monospace-font-family
  '("Iosevka"
    "Hack"
    "Fira Mono"
    "Droid Sans Mono"
    "Source Code Pro"
    "Dejavu Sans Mono")
  "List of monospace fonts to use")

(defvar adq/variable-font-family
  '("Fira Sans"
    "Noto Sans"
    "Dejavu Sans")
  "List of variable-width fonts to use")

(defvar adq/font-size
  (if (or (>= (display-pixel-width) 1680)
          (daemonp))
      11 8))

(defun adq/select-font (fonts fallback)
  "Selects the first available font from FONTS. If none of the fonts are available, returns FALLBACK."
  (or
   (--first (find-font (font-spec :name it)) fonts)
   fallback))

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%d"
                               (adq/select-font adq/monospace-font-family "Iosevka")
                               adq/font-size)))

;; Fix missing glyphs
(set-fontset-font "fontset-default" nil
                  (font-spec :size adq/font-size :name "Symbola"))

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (setq-local buffer-face-mode-face
;;                         `(:family ,(adq/select-font adq/variable-font-family "Fira Sans")))
;;             (buffer-face-mode)))

(bind-keys ("<C-mouse-4>" . text-scale-increase)
           ("<C-mouse-5>" . text-scale-decrease))

;; Make keywords always cursive
(add-hook 'adq/switch-theme-hook #'adq/setup-font-personalization)

(defun adq/setup-font-personalization ()
  (let ((comment-family "victor mono"))
    (set-face-attribute 'font-lock-keyword-face nil
                        :slant 'italic)
    (set-face-attribute 'font-lock-comment-face nil
                        :slant 'italic
                        :weight 'normal)
    (set-face-attribute 'font-lock-doc-face nil
                        :slant 'italic
                        :weight 'normal)
    (when (find-font (font-spec :name comment-family))
      (set-face-attribute 'font-lock-comment-face nil
                          :family comment-family)
      (set-face-attribute 'font-lock-doc-face nil
                          :family comment-family))))


(provide 'init-fonts)

;;; init-fonts.el ends here.
