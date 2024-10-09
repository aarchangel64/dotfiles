;;; ../../.dotfiles/doom/lang/web.el -*- lexical-binding: t; -*-

;; (setq-hook! 'web-mode-hook +format-with 'prettier-prettify)
(add-hook! 'web-mode-hook #'lsp #'tree-sitter-hl-mode)
;; (use-package! web-mode :mode (rx ".astro" string-end))

;; NOTE: astro-ls is exported as an npm dependency of astro, so you don't have to install it globally.

(setq lsp-tailwindcss-add-on-mode t)

;; (after!
;;   (web-mode tree-sitter)
;;   (when (string= (web-mode-detect-engine) "astro")
;;     ;; (setf (alist-get 'web-mode tree-sitter-major-mode-language-alist) 'astro)
;;     (tree-sitter-require 'astro)))


(use-package! astro-ts-mode
  :config
  (global-treesit-auto-mode)
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)
    (add-to-list 'treesit-auto-langs 'astro)))


(use-package! jsonc-mode :mode (rx (or ".json5" "tsconfig.json") string-end))

(after! (jsonc-mode tree-sitter) (tree-sitter-require 'json5))

;; (when (string= (file-name-extension buffer-file-name) "json5"))

;; smart-paren support
;; (defun my-web-mode-hook ()
;;   (setq web-mode-enable-auto-pairing nil))

;; (add-hook 'web-mode-hook  'my-web-mode-hook)

;; (defun sp-web-mode-is-code-context (id action context)
;;   (and (eq action 'insert)
;;        (not (or (get-text-property (point) 'part-side)
;;                 (get-text-property (point) 'block-side)))))

;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

(add-hook! 'typescript-mode-hook #'lsp)

;; _typescript.goToSourceDefinition
(defun cosmic/ts-ls-find-source (&key display-action)
  "Find source of the symbol under point."
  (interactive)
  (lsp-find-custom
   "_typescript.goToSourceDefinition"
   nil
   :display-action display-action))

(after!
  (evil tide)
  (evil-set-command-property 'cosmic/lsp-find-source :jump t)
  ;; Kind of a hack, use tide-mode to tell Doom when it should use the custom lookup.
  ;; Using tide-mode instead of typescript-mode because minor modes seem to always
  ;; stack on top of major modes, and the lsp lookup functions were overriding it when set to a major mode.
  (set-lookup-handlers! 'tide-mode :definition 'cosmic/ts-ls-find-source))

;; Fix for ts-ls until Emacs 29, see: https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
;; same definition as mentioned earlier
(advice-add
 'json-parse-string
 :around
 (lambda (orig string &rest rest)
   (apply orig (s-replace "\\u0000" "" string) rest)))

;; minor changes: saves excursion and uses search-forward instead of re-search-forward
(advice-add
 'json-parse-buffer
 :around
 (lambda (oldfn &rest args)
   (save-excursion
     (while (search-forward "\\u0000" nil t)
       (replace-match "" nil t)))
   (apply oldfn args)))
