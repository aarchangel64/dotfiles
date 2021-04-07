;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Shrey Pasricha"
      user-mail-address "shrey.pasricha@gmail.com")



;;; =================================== Visuals ===================================

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

(setq doom-font "Iosevka SS07-12"
      doom-variable-pitch-font "Iosevka Aile-12")

;; Default: 'doom-one'
;; My favourites: doom-henna, doom-horizon, doom-challenger-deep
(setq doom-theme 'doom-horizon)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; Set title frame to show current buffer name, save state, and project
(setq frame-title-format
      '(""
        (replace-regexp-in-string ".*/[0-9]*-?" "☰ " (subst-char-in-string ?_ ?  buffer-file-name))
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" "  ●  %s") project-name))))))

;; From https://tecosaur.github.io/emacs-config/config.html#which-key
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))



;;; ============================== MISC CONFIGURATION =============================

;; I needed this to get spell checking to work
(setq ispell-alternate-dictionary "/usr/bin/look")

;; agressively indent in these modes
(use-package! aggressive-indent
  :hook
  (org-mode . aggressive-indent-mode)
  (latex-mode . aggressive-indent-mode)
  (emacs-lisp-mode . aggressive-indent-mode))

;; Make PDFs (and SVGs?) look nicer (mayyybe)
(setq pdf-view-use-scaling t)

;; Open lookups in new window: https://github.com/hlissner/doom-emacs/issues/3397
;; (dolist (fn '(definition references documentation))
;;   (fset (intern (format "+lookup/%s-new-window" fn))
;;         (lambda (identifier &optional arg)
;;           "TODO"
;;           (interactive (list (doom-thing-at-point-or-region)
;;                              current-prefix-arg))
;;           (let ((pt (point)))
;;             (+evil-window-vsplit-a (get-buffer-window))
;;             (switch-to-buffer-other-window (current-buffer))
;;             (goto-char pt)
;;             (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

;;; =========================== LANGUAGE CONFIGURATIONS ===========================

;; Lua / Fennel
(use-package! fennel-mode
  :mode "\\.fnl\\'"
  :config)

(after! mode-local
  (add-hook 'fennel-mode-hook
            (lambda ()
              (setq-mode-local fennel-mode compile-command '(format "fennel --compile %1$s.fnl > %1$s.lua" (file-name-sans-extension (buffer-file-name)))))))

;; (add-hook 'lua-mode-hook #'lsp!)
;; (after! lsp-lua
;;   (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server")
;;   (setq lsp-clients-lua-language-server-install-dir "/usr/bin/")
;;   )

;; Arch Linux PKGBUILD
(use-package! pkgbuild-mode
  :mode "/PKGBUILD$")

;; Rust
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

;; C/C++
(setq lsp-clients-clangd-args '("-j=16"
                                "--enable-config"
                                "--background-index"
                                ;; "--clang-tidy"
                                "--completion-style=detailed"
                                "--cross-file-rename"
                                "--suggest-missing-includes"
                                "--recovery-ast"
                                "--header-insertion=never"
                                "--fallback-style=Microsoft"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; Fish shell
(add-hook! fish-mode
  (set-company-backend! 'fish-mode '(company-shell company-shell-env company-fish-shell company-files)))



;;; =============================== External Configs ==============================
;; (load! "project")
(load! "mapping")
(load! "latex-config")
