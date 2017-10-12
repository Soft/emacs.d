;;; init-help.el --- Help related functions -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs is a complex editor. Let's make accessing help as easy as we can.

;;; Code:

;; helpful is really nice but sometimes it fails with an error. This macro adds
;; an advice to try an alternative (in this case likely from the describe-*
;; family of functions) if the first function fails.
(defmacro adq/add-alternative-interactive (fn alternative)
  `(advice-add
    (quote ,fn) :around
    (lambda (fn &rest args)
      (condition-case nil
          (funcall-interactively fn (car args))
        (error (funcall-interactively (quote ,alternative) (car args)))))))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(defvar adq/helpful-reuse-buffers t
  "Should helpful reuse existing buffers.")

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-function)
   ("C-h v" . helpful-variable)
   ("C-h c" . helpful-command)
   ("C-h SPC" . helpful-at-point))
  :config
  (adq/add-alternative-interactive helpful-function describe-function)
  (adq/add-alternative-interactive helpful-variable describe-variable)
  (adq/add-alternative-interactive helpful-command describe-command)
  ;; I like helpful to reuse existing helpful-mode buffers.
  (advice-add
   'helpful--buffer :around
   (lambda (fn &rest args)
     (let ((original-get-buffer-create (symbol-function 'get-buffer-create)))
       (flet
           ((get-buffer-create
             (name)
             (if-let ((_ adq/helpful-reuse-buffers)
                      (buffer (car (adq/buffers-with-major-mode 'helpful-mode))))
                 (progn
                   (with-current-buffer buffer
                     (rename-buffer name))
                   buffer)
               (funcall original-get-buffer-create name))))
         (apply fn args)))))
  (bind-keys
   :map helpful-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("h" . left-char)
   ("l" . right-char)
   ("J" . scroll-down)
   ("K" . scroll-up)))

(bind-key "C-h K" #'describe-personal-keybindings)

(use-package eldoc
  :diminish eldoc-mode
  :defer t)

(defconst adq/adequate-url "https://bitbucket.org/Soft/emacs.d/src")

(defun adq/about-adequate-emacs-d ()
  (interactive)
  (browse-url adq/adequate-url))

(define-key-after
  (lookup-key global-map [menu-bar help-menu])
  [adequate-website]
  '("About Adequate emacs.d" . adq/about-adequate-emacs-d)
  'about-gnu-project)

(use-package keyfreq
  :ensure t
  :init
  (setq keyfreq-file (f-join user-emacs-directory "keyfreq")
        keyfreq-file-lock (f-join user-emacs-directory "keyfreq.lock"))
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(provide 'init-help)

;;; init-help.el ends here
