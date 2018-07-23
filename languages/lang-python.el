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

(use-package anaconda-mode
  :ensure t
  :defer t)

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
  (anaconda-eldoc-mode)
  (pyvenv-mode)
  ;; (when-let ((venv (adq/python-find-project-venv)))
  ;;   (pyvenv-activate venv)
  ;;   (message "Activated virtual environment %s" venv))
  (when (and (equal (buffer-name) "setup.py")
             (eq (buffer-size) 0))
    (auto-insert)))

(define-skeleton adq/python-skeleton-doc-comment
  "Insert Python doc comment." nil
  > "\"\"\"" _ "\"\"\"" \n)

(defvar adq/pypi-address "https://pypi.org"
  "Python package index address.")

(defun adq/python-pypi-package-info (project &optional version)
  "Retrieve package info from Python package index."
  (cl-block query
    (request (if version
                 (format "%s/pypi/%s/%s/json" adq/pypi-address project version)
               (format "%s/pypi/%s/json" adq/pypi-address project))
             :parser 'json-read
             :sync t
             :error
             (lambda (&rest _) (cl-return-from query))
             :success
             (cl-function
              (lambda (&key data &allow-other-keys)
                (cl-return-from query data))))))

(defvar adq/python-classifier-cache '()
  "Cached Python package classifiers.")

(defun adq/python-get-classifiers ()
  "Returns list of Python package classifiers."
  (unless adq/python-classifier-cache
    (request (concat adq/pypi-address "/pypi?%3Aaction=list_classifiers")
             :parser 'buffer-string
             :sync t
             :success
             (cl-function
              (lambda (&key data &allow-other-keys)
                (setq adq/python-classifier-cache (s-lines data))))))
  adq/python-classifier-cache)

(defun adq/python-get-dependency (package &optional version)
  "Return package dependency defition. When `version' is not
specified, the latest version available on Python package index
is used."
  (when-let (version
             (or version
                 (let-alist (adq/python-pypi-package-info package)
                   .info.version)))
    (concat package "==" version)))

(defun adq/python-insert-dependency (package &optional version)
  "Insert package dependency definition. When `version' is not
specified, the latest version available on Python package index
is used."
  (if-let (definition (adq/python-get-dependency package version))
      (insert definition)
    (error "Could not find package %s" package)))

(define-skeleton adq/python-skeleton-setup
  "Insert Python setup.py template." nil
  "#!/usr/bin/env python" \n \n
  "from setuptools import setup, find_packages" \n \n
  "setup(name=\""
  (when (buffer-file-name)
    (setq v1 (f-filename (f-dirname (buffer-file-name))))) |
  (setq v1 (skeleton-read "Name: ")) "\"," \n
  > "version=\"" (skeleton-read "Version: ") | "0.1" "\"," \n
  > "description=\"" (skeleton-read "Description: ") "\"," \n
  > "long_description=\"\"," \n
  > "packages=find_packages()," \n
  > "entry_points={" \n
  > "\"console_scripts\": [" \n
  > "\"" v1 "=" v1 ":main\"" \n
  > "]" \n
  > "}," \n
  > "install_requires=[" \n
  ((adq/python-get-dependency (skeleton-read "Add Dependency: " nil t)) > "\"" str "\"," \n)
  > "]," \n
  > "keywords=[" \n
  ((completing-read "Add Keywords: " (adq/python-get-classifiers) nil nil) > "\"" str "\"," \n)
  > "])" \n \n _)

(define-auto-insert "setup\\.py\\'" 'adq/python-skeleton-setup)

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

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode))
  :init (add-hook 'python-mode-hook #'adq/python-setup)
  :bind (:map python-mode-map
              ("M-\"" . adq/python-skeleton-doc-comment)))

(provide 'lang-python)

;;; lang-python.el ends here
