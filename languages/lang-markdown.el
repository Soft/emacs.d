
(defun make-compiler (command name-transformer args-maker)
  (let* ((command-base (file-name-base command))
         (process-name (concat command-base "-process"))
         (buffer-name (format "*%s-Log*" (capitalize command-base)))
         (args (list process-name buffer-name command)))
    (lambda ()
      (interactive)
      (let* ((name (file-name-base buffer-file-name))
             (dir (file-name-directory buffer-file-name))
             (output (funcall name-transformer
                              (concat (file-name-as-directory dir) name)))
             (proc (apply #'start-process
                          (cl-concatenate 'list args (funcall args-maker output)))))
        (message "%s: %s" (capitalize command-base) output)
        (process-send-string proc (buffer-string))
        (process-send-eof proc)))))

(fset 'pandoc-pdf-from-buffer
      (make-compiler "pandoc"
                     (partial concat _ ".pdf")
                     (lambda (output) `("-o" ,output "-f" "markdown"))))
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (progn
    (setq markdown-bold-underscore t
          markdown-enable-math t
          markdown-command "pandoc")
    (cl-loop for i from 1 to 6
             do (set-face-attribute
                 (intern (format "markdown-header-face-%d" i)) nil :height (+ 1.0 (/ 1.0 i))))
    (set-face-attribute 'markdown-blockquote-face nil :slant 'italic)))

(provide 'lang-markdown)
