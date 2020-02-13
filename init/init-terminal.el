;;; init-terminal.el --- Shells and terminals -*- lexical-binding: t -*-

;;; Commentary:

;; Terminals and shells.

;;; Code:

(use-package multi-term
  :ensure t
  :defer t)

(defun adq/get-multi-term ()
  "Switch to an existing terminal buffer or open a new one."
  (interactive)
  (adq/switch-or-call
   (adq/compose 'car (adq/partial adq/buffers-with-major-mode 'term-mode))
   'multi-term))

(defun adq/get-eshell ()
  "Switch to an existing eshell buffer or open a new one."
  (interactive)
  (adq/switch-or-call
   (adq/compose 'car (adq/partial adq/buffers-with-major-mode 'eshell-mode))
   'eshell))

(defvar adq/prefer-eshell t
  "Should command that launch or retrieve terminals prefer eshell over multi-term.")

(defun adq/get-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if adq/prefer-eshell
       (adq/switch-command #'adq/get-eshell #'adq/get-multi-term)
     (adq/switch-command #'adq/get-multi-term #'adq/get-eshell))
   d))

(defun adq/new-shell-like (d)
  (interactive "p")
  (funcall-interactively
   (if adq/prefer-eshell
       (adq/switch-command #'eshell #'multi-term)
     (adq/switch-command #'multi-term #'eshell))
   d))

(bind-keys
 ("C-c <return>" . adq/get-shell-like)
 ("C-c <C-return>" . adq/new-shell-like))

(defun adq/comint-clear-buffer ()
  "Clear current comint buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(use-package comint
  :bind (:map comint-mode-map
              ("C-l" . adq/comint-clear-buffer)
              ("C-d" . quit-window))
  :config
  (add-hook
   'comint-exec-hook
   (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

  (setq comint-input-ring-size 10000)

  ;; These definitions would make more sense in `init-session.el' but comint
  ;; configuration is here.
  (defun adq/comint-enable-persistent-history ()
    (when-let (process (get-buffer-process (current-buffer)))
      (let* ((comint-history-dir (f-join user-emacs-directory "comint"))
             (comint-history-file (f-join comint-history-dir
                                          (format "%s-history" (md5 (process-name process))))))
        (unless (f-directory? comint-history-dir)
          (f-mkdir comint-history-dir))
        (setq-local comint-input-ring-file-name comint-history-file)
        (when (f-file? comint-history-file)
          (comint-read-input-ring))
        ;; This might actually not be necessary as `kill-buffer-hook' seems to
        ;; do the job.
        (set-process-sentinel
         process
         (lambda (process _)
           (when-let ((buffer (process-buffer process)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (comint-write-input-ring)))))))))

  (add-hook 'comint-mode-hook #'adq/comint-enable-persistent-history)
  (add-hook
   'kill-buffer-hook
   (lambda ()
     (when (derived-mode-p 'comint-mode)
       (comint-write-input-ring))))
  (add-hook
   'kill-emacs-hook
   (lambda ()
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (when (derived-mode-p 'comint-mode)
           (comint-write-input-ring)))))))

(use-package em-term
  :config
  (adq/add-to-list-many 'eshell-visual-commands '("most"))
  (adq/add-to-list-many 'eshell-visual-subcommands '(("git" ("log" "diff" "show")))))

;; Eshell prompt

;; TODO: Use defface

(defvar adq/eshell-path-faces
  '((:foreground "#9400d3")
    (:foreground "#4b0082")
    (:foreground "#0000ff")
    (:foreground "#00ff00")
    (:foreground "#ffff00")
    (:foreground "#ff7f00")
    (:foreground "#ff0000"))
  "Faces used for path components in eshell.")

(defvar adq/eshell-path-separator-face
  '(:foreground "#cccccc")
  "Face for path separators in eshell.")

(defvar adq/eshell-user-face
  '(:foreground "#ffffff")
  "Face for user names in eshell.")

(defvar adq/eshell-machine-face
  '(:foreground "#f442d4")
  "Face for user names in eshell.")

(defvar adq/eshell-at-face
  '(:foreground "#42f4df")
  "Face for at sign in eshell.")

(defvar adq/eshell-suffix-face
  '(:foreground "#f442d4")
  "Face for suffix in eshell.")

(defvar adq/eshell-git-face-alist
  '((modified . (:foreground "#b342ff"))
    (added . (:foreground "#00ff00"))
    (deleted . (:foreground "#ff004c"))
    (renamed . (:foreground "#ffe500"))
    (copied . (:foreground "#ff8142"))
    (untracked . (:foreground "#00c3ff")))
  "Faces for git indicators in eshell.")

;; This is most likely broken on Windows
(defun adq/eshell-format-path (path)
  (let* ((full-path (f-full path))
         (separator (f-path-separator))
         (components (-remove 's-blank? (s-split separator full-path)))
         (colors (-take (length components)
                        (-cycle adq/eshell-path-faces)))
         (colored-sep (propertize separator 'font-lock-face adq/eshell-path-separator-face)))
    (s-prepend
     colored-sep
     (s-join colored-sep
             (-zip-with (lambda (p c) (propertize p 'font-lock-face c))
                        components
                        colors)))))

(defun adq/eshell-format-user-and-machine ()
  (concat (propertize (user-login-name) 'font-lock-face adq/eshell-user-face)
          (propertize "@" 'font-lock-face adq/eshell-at-face)
          (propertize system-name 'font-lock-face adq/eshell-machine-face)))

(defun adq/eshell-format-git-component (stat table)
  (let ((value (length (gethash stat table)))
        (icon (pcase stat
                ('modified "🅼")
                ('added "🅰")
                ('deleted "🅳")
                ('renamed "🆁")
                ('copied "🅲")
                ('untracked "🆄")))
        (face (cdr (assoc stat adq/eshell-git-face-alist))))
    (when (> value 0)
      (list (propertize (format "%s%d" icon value) 'font-lock-face face)))))

(defun adq/eshell-format-git ()
  (let* ((status (adq/git-repository-status (eshell/pwd)))
         (stats '(modified added deleted renamed copied untracked))
         (components (--mapcat (adq/eshell-format-git-component it status) stats))
         (result (s-join "" components)))
    (if (not (s-blank? result)) 
        (format "[%s]:" result)
      "")))

(defun adq/eshell-format-prompt ()
  (let ((string
         (concat (adq/eshell-format-user-and-machine)
                 ":"
                 (adq/eshell-format-git)
                 (adq/eshell-format-path (eshell/pwd))
                 (propertize  " »" 'font-lock-face adq/eshell-suffix-face)
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

(defun adq/eshell-setup ()
  (eshell-fringe-status-mode)
  (with-editor-export-editor)
  (company-mode -1) ;; Even better would be to only disable company when using TRAMP since it can be slow
  (setenv "TERM" "xterm-256color")
  (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
  (setq xterm-color-preserve-properties t
        eshell-output-filter-functions (remove 'eshell-handle-ansi-color
                                               eshell-output-filter-functions))
  (bind-keys
   :map eshell-mode-map
   ("C-d" . quit-window)))

(defun eshell/em (&rest args)
  "Open files in Emacs."
  (if (null args)
      (bury-buffer)
    (-each  (-map #'expand-file-name (eshell-flatten-list args)) #'find-file)))

;; This is quite drastic but messing up eshell by running git diff isn't nice.
(defun eshell/git (&rest args)
  "Launch Magit based on git subcommand."
  (if-let ((command (car (eshell-flatten-list args)))
           (handler (pcase command
                      ("diff" 'magit-diff-popup)
                      ("log" 'magit-log-popup)
                      ("commit" 'magit-commit-popup)
                      ("branch" 'magit-branch-popup)
                      ("push" 'magit-push-popup)
                      ("pull" 'magit-fetch-popup)
                      ("cherry-pick" 'magit-cherry-pick-popup)
                      ("reset" 'magit-reset-popup)
                      ("status" 'magit-status)
                      (_ (error "Unknown command: %s" command)))))
      (call-interactively handler)
    (magit-status)))

(use-package eshell
  :defer t
  :init
  (setq eshell-prompt-function 'adq/eshell-format-prompt
        eshell-prompt-regexp "^[^»]* » "
        eshell-banner-message ""
        eshell-highlight-prompt nil)
  :config
  (add-hook 'eshell-mode-hook #'adq/eshell-setup))

(provide 'init-terminal)

;;; init-terminal.el ends here
