;;; ../../.dotfiles/doom/lang/c.el -*- lexical-binding: t; -*-

;;; ============================= Clangd LSP settings =============================

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(setq c-doc-comment-style '((java-mode . javadoc)
                            (pike-mode . autodoc)
                            (c-mode    . gtkdoc)
                            (c++-mode  . doxygen)))


(after! lsp-clangd
  (set-lsp-priority! 'clangd 2)
  (setq lsp-clients-clangd-args '("-j=16"
                                  "--enable-config"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--fallback-style=llvm")))

(add-hook! 'c-mode-common-hook #'clang-format+-mode)
(after! clang-format
  (setq clang-format+-always-enable t
        clang-format-fallback-style "llvm"))
