;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; Shells and terminals

(use-package multi-term
  :ensure t
  :defer t)

(defun get-multi-term ()
  "Switch to an existing terminal buffer or open a new one."
  (interactive)
  (switch-or-call
   (compose 'car (partial buffers-with-major-mode 'term-mode))
   'multi-term))

(defun get-eshell ()
  "Switch to an existing eshell buffer or open a new one."
  (interactive)
  (switch-or-call
   (compose 'car (partial buffers-with-major-mode 'eshell-mode))
   'eshell))

(defvar prefer-eshell t
  "Should command that launch or retrieve terminals prefer eshell over multi-term.")

(defun get-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if prefer-eshell
       (switch-command (get-eshell) (get-multi-term))
     (switch-command (get-multi-term) (get-eshell)))
   d))

(defun new-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if prefer-eshell
       (switch-command (eshell) (multi-term))
     (switch-command (multi-term) (eshell)))
   d))

(bind-keys
 ("C-c <return>" . get-shell-like)
 ("C-c <C-return>" . new-shell-like))

(defun comint-clear-buffer ()
  "Clear current comint buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(use-package comint
  :config
  (add-hook
   'comint-exec-hook
   (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))
  (bind-keys
   :map comint-mode-map
   ("C-l" . comint-clear-buffer)
   ("C-d" . kill-this-buffer)))

(use-package em-term
  :config
  (add-to-list-many 'eshell-visual-commands '("most"))
  (add-to-list-many 'eshell-visual-subcommands '(("git" ("log" "diff" "show")))))

;; Eshell prompt

(defvar eshell-path-faces
  '((:foreground "#9400d3")
    (:foreground "#4b0082")
    (:foreground "#0000ff")
    (:foreground "#00ff00")
    (:foreground "#ffff00")
    (:foreground "#ff7f00")
    (:foreground "#ff0000"))
  "Faces used for path components in eshell.")

(defvar eshell-path-separator-face
  '(:foreground "#cccccc")
  "Face for path separators in eshell.")

(defvar eshell-user-face
  '(:foreground "#ffffff")
  "Face for user names in eshell.")

(defvar eshell-machine-face
  '(:foreground "#f442d4")
  "Face for user names in eshell.")

(defvar eshell-at-face
  '(:foreground "#42f4df")
  "Face for at sign in eshell.")

(defvar eshell-suffix-face
  '(:foreground "#f442d4")
  "Face for suffix in eshell.")

(defvar eshell-git-face-alist
  '((modified . (:foreground "#b342ff"))
    (added . (:foreground "#00ff00"))
    (deleted . (:foreground "#ff004c"))
    (renamed . (:foreground "#ffe500"))
    (copied . (:foreground "#ff8142"))
    (untracked . (:foreground "#00c3ff")))
  "Faces for git indicators in eshell.")

;; This is most likely broken on Windows
(defun eshell-format-path (path)
  (let* ((full-path (f-full path))
         (separator (f-path-separator))
         (components (-remove 's-blank? (s-split separator full-path)))
         (colors (-take (length components)
                        (-cycle eshell-path-faces)))
         (colored-sep (propertize separator 'font-lock-face eshell-path-separator-face)))
    (s-prepend
     colored-sep
     (s-join colored-sep
             (-zip-with (lambda (p c) (propertize p 'font-lock-face c))
                        components
                        colors)))))

(defun eshell-format-user-and-machine ()
  (concat (propertize (user-login-name) 'font-lock-face eshell-user-face)
          (propertize "@" 'font-lock-face eshell-at-face)
          (propertize system-name 'font-lock-face eshell-machine-face)))

(defun eshell-format-git-component (stat table)
  (let ((value (length (gethash stat table)))
        (icon (pcase stat
                ('modified "🅼")
                ('added "🅰")
                ('deleted "🅳")
                ('renamed "🆁")
                ('copied "🅲")
                ('untracked "🆄")))
        (face (cdr (assoc stat eshell-git-face-alist))))
    (when (> value 0)
      (list (propertize (format "%s%d" icon value) 'font-lock-face face)))))

(defun eshell-format-git ()
  (let* ((status (git-repository-status (eshell/pwd)))
         (stats '(modified added deleted renamed copied untracked))
         (components (--mapcat (eshell-format-git-component it status) stats))
         (result (s-join "" components)))
    (if (not (s-blank? result)) 
        (format "[%s]:" result)
      "")))

(defun eshell-format-prompt ()
  (let ((string
         (concat (eshell-format-user-and-machine)
                 ":"
                 (eshell-format-git)
                 (eshell-format-path (eshell/pwd))
                 (propertize  " »" 'font-lock-face eshell-suffix-face)
                 " ")))
    (add-text-properties 0 (length string)
                         '(read-only t rear-nonsticky (face read-only))
                         string)
    string))

(use-package xterm-color
  :ensure t
  :defer t
  :commands (xterm-color-filter))

(use-package eshell-fringe-status
  :defer t
  :ensure t)

(defun eshell-setup ()
  (eshell-fringe-status-mode)
  (with-editor-export-editor)
  (setenv "TERM" "xterm-256color")
  (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
  (setq xterm-color-preserve-properties t
        eshell-output-filter-functions (remove 'eshell-handle-ansi-color
                                               eshell-output-filter-functions))
  (bind-keys
   :map eshell-mode-map
   ("C-d" . kill-this-buffer)))

(use-package eshell
  :defer t
  :init
  (setq eshell-prompt-function 'eshell-format-prompt
        eshell-prompt-regexp "^[^»]* » "
        eshell-highlight-prompt nil)
  :config
  (add-hook 'eshell-mode-hook #'eshell-setup))

(provide 'init-terminal)

