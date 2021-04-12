;;; ../../.dotfiles/doom/lang/c.el -*- lexical-binding: t; -*-

;;; ============================= Clangd LSP settings =============================

(after! lsp-clangd
  (set-lsp-priority! 'clangd 2)
  (setq lsp-clients-clangd-args '("-j=16"
                                  "--enable-config"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--cross-file-rename"
                                  "--suggest-missing-includes"
                                  "--recovery-ast"
                                  "--header-insertion=never"
                                  "--fallback-style=Microsoft")))
