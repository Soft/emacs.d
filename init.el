;;; init.el -*- lexical-binding: t; -*-

(require 'init-adequate
         (concat adq/adequate-directory "init/init-adequate"))

(adq/init
 ;; Basic configuration
 config-libraries
 config-prelude
 config-local
 config-interface
 config-modeline
 config-fonts
 config-themes
 config-dashboard
 config-editor
 config-vim
 config-windows
 config-buffers
 config-session
 config-backups
 config-projects
 config-help
 config-search
 config-completion
 config-git
 config-repl
 config-term
 config-check
 config-lsp
 config-tools
 ;; Language specific modules
 env-lisp
 env-cc
 env-rust
 env-go
 env-python
 env-haskell
 env-web
 env-markdown
 env-org
 env-tex
 env-misc-linux
 env-misc)
