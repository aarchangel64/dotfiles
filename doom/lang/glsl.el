;;; ../../.dotfiles/doom/lang/glsl.el -*- lexical-binding: t; -*-

;; TODO: Setup getting man page
;; (after! glsl-mode
;;   (set-lookup-handlers! 'glsl-mode :documentation #'glsl-find-man-page))


(after! (glsl-mode lsp-mode)
  (add-to-list 'lsp-language-id-configuration
               '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("~/Build/glsl-language-server/build/glslls" "--stdin"))
                    :activation-fn (lsp-activate-on "glsl")
                    :server-id 'glslls)))

(after! (glsl-mode tree-sitter)
  (tree-sitter-require 'glsl))

(add-hook! glsl-mode #'lsp #'tree-sitter-hl-mode)
