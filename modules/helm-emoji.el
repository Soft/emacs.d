;;; helm-emoji.el --- Emoji picker for Helm -*- lexical-binding: t -*-

;; Package-Requires: ((helm) (emacs "24.3"))

;;; Commentary:

;; Emoji picker for Helm.

;;; Code:

(require 'cl-lib)

(defconst helm-emoji-ranges
  '((#x1F300 . #x1F5FF) ; Miscellaneous Symbols and Pictograms
    (#x1F600 . #x1F64F) ; Emoticons
    (#x1F680 . #x1F6F3) ; Transport and Map Symbols
    (#x2600  . #x26FF)  ; Miscellaneous Symbols
    (#x2700  . #x27BF)) ; Dingbats
  "Code point ranges that contain emoji characters.")

(defvar helm-emoji-cache nil)

(defface helm-emoji-emoji-face
  '((t . (:foreground "steel blue")))
  "Face used for displaying emoji."
  :group 'helm)

(defface helm-emoji-name-face
  '((t . ()))
  "Face used for displaying names of emoji."
  :group 'helm)

(defun helm-emoji-range (start stop)
  (cl-loop for i from start to stop
           when (get-char-code-property i 'name)
           collect (cons
                    (format "%s %s"
                            (propertize (char-to-string i) 'face 'helm-emoji-emoji-face)
                            (propertize (capitalize it) 'face 'helm-emoji-name-face))
                    i)))

(defvar helm-source-emoji
  '((name . "Emoji")
    (candidates . (lambda ()
                    (unless helm-emoji-cache
                      (setq helm-emoji-cache
                            (apply #'cl-concatenate 'list
                                   (cl-loop for (start . stop) in helm-emoji-ranges
                                            collect (helm-emoji-range start stop)))))
                    helm-emoji-cache))
    (action . insert)))

;;;###autoload
(defun helm-emoji ()
  "Select emoji using Helm."
  (interactive)
  (helm-other-buffer 'helm-source-emoji nil))

(provide 'helm-emoji)

;;; helm-emoji.el ends here
