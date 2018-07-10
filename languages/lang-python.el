;;; lang-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:

;; Python programming environment

;;; Code:

(use-package pyvenv
  :ensure t
  :defer t)

(use-package yapfify
  :if (adq/programs-p "yapf")
  :ensure t
  :defer t)

(defun adq/python-setup ()
  "Defaults for Python."
  (highlight-indent-guides-mode)
  (anaconda-eldoc-mode)
  (pyvenv-mode)
  ;; (when-let ((venv (adq/python-find-project-venv)))
  ;;   (pyvenv-activate venv)
  ;;   (message "Activated virtual environment %s" venv))
  )

(define-skeleton adq/python-doc-comment
  "Insert Python doc comment." nil
  > "\"\"\"" _ "\"\"\"" \n)

(defun adq/python-find-project-venv (&optional project)
  "Returns virtual environment directory of PROJECT or, if nil,
the current project. Returns nil if no virtual environment is
found."
  (cl-block find-venv
    (dolist (root (project-roots (or project
                                     (project-current)
                                     (cl-return-from find-venv))))
      (f-files root
               (lambda (file)
                 (when (equal (f-filename file) "pyvenv.cfg")
                   (cl-return-from find-venv (f-dirname file))))
               t))))

(defun adq/python-venv-activate (&optional project)
  "Find and activate virtual environment for the project."
  (interactive)
  (if-let (venv (adq/python-find-project-venv project))
      (progn
        (pyvenv-activate venv)
        (message "Activated virtual environment %s" venv))
    (error "Cannot find virtual environment")))

(use-package anaconda-mode
  :ensure t
  :defer t)

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :init (add-hook 'python-mode-hook #'adq/python-setup)
  :bind (:map python-mode-map
              ("M-\"" . adq/python-doc-comment)))

(provide 'lang-python)

;;; lang-python.el ends here
