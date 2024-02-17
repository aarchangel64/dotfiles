;;; ../../.dotfiles/doom/lang/elisp.el -*- lexical-binding: t; -*-

;; TODO: Configure elisp autoformat
;; see https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package!
    elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; TODO: remove smart paren based on indent level??
