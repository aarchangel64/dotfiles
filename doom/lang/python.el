;;; ../../.dotfiles/doom/lang/python.el -*- lexical-binding: t; -*-

(use-package! sphinx-doc-mode
  :hook python-mode
  :config
  (setq sphinx-doc-include-types t))

(setq-hook! 'python-mode-hook +format-with-lsp nil)

(map! :map python-mode-map
      :localleader
      :desc "Generate Doc-Strings" "d" #'sphinx-doc)
