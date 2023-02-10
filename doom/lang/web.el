;;; ../../.dotfiles/doom/lang/web.el -*- lexical-binding: t; -*-

;; (setq-hook! 'web-mode-hook +format-with 'prettier-prettify)
(add-hook! 'web-mode-hook #'prettier-js-mode #'tree-sitter-hl-mode)
(use-package! web-mode
  :mode (rx ".astro" string-end))

(setq lsp-tailwindcss-add-on-mode t)

(after! (web-mode tree-sitter)
  (when (string= (web-mode-detect-engine) "astro")
    ;; (setf (alist-get 'web-mode tree-sitter-major-mode-language-alist) 'astro)
    (tree-sitter-require 'astro)))

(use-package! jsonc-mode
  :mode (rx (or ".json5" "tsconfig.json") string-end))

(after! (jsonc-mode tree-sitter)
  (tree-sitter-require 'json5))

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
  (lsp-find-custom "_typescript.goToSourceDefinition" nil :display-action display-action))

(after! (evil lsp-mode lsp-ui typescript-mode)
  (evil-set-command-property 'cosmic/lsp-find-source :jump t)
  (set-lookup-handlers! 'lsp-ui-mode :definition nil)
  (set-lookup-handlers! 'lsp-mode :definition nil)
  (set-lookup-handlers! 'typescript-mode
    :definition 'cosmic/ts-ls-find-source))

;; Fix for ts-ls until Emacs 29, see: https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
;; same definition as mentioned earlier
(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))

;; minor changes: saves excursion and uses search-forward instead of re-search-forward
(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
	      (save-excursion
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
	      (apply oldfn args)))
