;;; ../../.dotfiles/doom/lang/c.el -*- lexical-binding: t; -*-

;;; ============================= Clangd LSP settings =============================

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

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
                                  "--fallback-style=llvm")))

(add-hook! 'c-mode-common-hook #'clang-format+-mode)
(setq clang-format+-always-enable t
      clang-format-fallback-style "llvm")
