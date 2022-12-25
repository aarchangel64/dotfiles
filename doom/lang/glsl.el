;;; ../../.dotfiles/doom/lang/glsl.el -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("~/Build/glsl-language-server/glslls" "--stdin"))
                    :activation-fn (lsp-activate-on "glsl")
                    :server-id 'glslls)))

(add-hook! glsl-mode #'lsp)
