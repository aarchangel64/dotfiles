;;; ../../.dotfiles/doom/lang/julia.el -*- lexical-binding: t; -*-

(after! julia-mode
  ;; https://docs.doomemacs.org/v21.12/#/prerequisites/language-server/lsp-julia
  (setq lsp-julia-package-dir nil)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.9")
  (setq lsp-enable-folding t))
