;;; init-debugging.el --- Debugging -*- lexical-binding: t -*-

;;; Commentary:

;; Debugger configuration

;;; Code:

;; TODO: Evil compatibility
(use-package realgud
  :ensure t
  :commands (realgud:pdb
             realgud:gdb
             realgud:jdb
             realgud:ipdb
             realgud:perldb
             realgud:remake
             realgud:bashdb
             realgud:node-debug
             realgud:trepan
             realgud:trepan2
             realgud:trepan3k
             realgud:trepanjs))

(provide 'init-debugging)

;;; init-debugging.el ends here
