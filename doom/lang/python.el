;;; ../../.dotfiles/doom/lang/python.el -*- lexical-binding: t; -*-

(use-package! sphinx-doc-mode
  :hook python-mode
  :config
  (setq sphinx-doc-include-types t))

(map! :map python-mode-map
      :localleader
      :desc "Generate Doc-Strings" "d" #'sphinx-doc)
