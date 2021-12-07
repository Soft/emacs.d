;;; env-rust.el -*- lexical-binding: t; -*-

(use-package pyvenv :defer t)

(use-package py-isort :defer t)

(use-package blacken
  :after python
  :bind (:map python-mode-map
              ("C-c c f" . blacken-buffer)))

(defvar adq/python-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("and" . ?∧)
    ("or" . ?∨)
    ("==" . ?≡)
    ("!=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("sum" . ?Σ)
    ("all" . ?∀)
    ("any" . ?∃))
  "Symbol prettification alist for `python-mode'.")

(defun adq/python-setup ()
  "Defaults for Python."
  (setq-local prettify-symbols-alist adq/python-prettify-symbols-alist)
  (highlight-indent-guides-mode)
  (pyvenv-mode))

(define-skeleton adq/python-skeleton-doc-comment
  "Insert Python doc comment." nil
  > "\"\"\"" _ "\"\"\"" \n)

(defun adq/python-format-docstring-at-point ()
  "Format Python docstring at point. Adjust the length of lines
taking indentation into account when deciding when to break lines."
  (interactive)
  (save-excursion
    (if-let ((quote-start (search-backward "\"\"\"" nil t))
             (quote-end (search-forward "\"\"\"" nil t 2))
             (line-start
              (save-excursion
                (goto-char quote-start)
                (re-search-backward "^" nil t)
                (point)))
             (indent (- quote-start line-start))
             (start (+ quote-start 3))
             (end (- quote-end 3)))
        (progn
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (adq/re-search-forward-replace
             (rx (group line-start (1+ whitespace))) "")
            (let ((fill-column (- fill-column (+ indent 1))))
              (fill-region (point-min) (point-max)))
            (goto-char (point-min))
            (adq/re-search-forward-replace
             (rx (group line-start))
             (make-string indent ?\s)))
          (delete-trailing-whitespace
           quote-start quote-end))
      (error "No docstring at point"))))

(defvar adq/python-venv-dirname "env"
  "Name for virtual environments created with
`adq/python-setup-venv'.")

(cl-defun adq/python-setup-venv (&key (project nil)
                                      (site-packages nil)
                                      (activate nil))
  "Setup new virtual environment for a project. If `project' is
defined, it will be used for finding the project root, otherwise
the current project will be used. If `site-packages' is non nil,
the newly-created virtual environment will have access to the
system site packages. If `activate' is non nill, the
newly-created virtual environment is activated."
  (interactive)
  (if-let (root (car (project-roots (or project
                                        (project-current)))))
      (let ((env (f-join root adq/python-venv-dirname)))
        (if (not (f-exists? env))
            (let ((args (-concat (list "python3" nil nil nil "-m" "venv")
                                 (if site-packages '("--system-site-packages") '())
                                 (list env))))
              (pcase (apply #'call-process args)
                (`0 (progn
                      (message "Virtual environment created at %s" env)
                      (when activate
                        (pyvenv-activate env))))
                (_ (error "Failed to create virtual environment at %s" env))))
          (error "%s already exists" env)))
    (error "Could not find project root")))

(defun adq/python-find-project-venv (&optional project)
  "Returns virtual environment directory of `project' or, if nil,
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

(use-package python
  :straight nil
  :mode (("\\.py\\'" . python-mode)
         ("/SConscript\\'" . python-mode)
         ("/SConstruct\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :bind
  (:map python-mode-map
        ("M-\"" . adq/python-skeleton-doc-comment)
        ("C-c c j" . run-python)
        ("C-c c r" . python-shell-send-region)
        ("C-c c b" . python-shell-send-buffer)
        ("C-c c d" . python-shell-send-defun))
  :config
  (add-hook 'python-mode-hook #'adq/python-setup))

(provide 'env-python)
