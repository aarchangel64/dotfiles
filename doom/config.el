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

(setq doom-font "Iosevka 14"
      doom-variable-pitch-font "Iosevka Aile 14")

;; Default: 'doom-one'
;; My favourites: doom-henna, doom-horizon, doom-challenger-deep
(setq doom-theme 'doom-horizon)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

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

;; Make PDFs (and SVGs?) look nicer (mayyybe)
(setq pdf-view-use-scaling t)
;; (pgtk-set-monitor-scale-factor "DP-2" 1.0)

;; agressively indent in these modes
(use-package! aggressive-indent-mode
  :hook
  (
   org-mode
   lua-mode
   latex-mode
   fennel-mode
   python-mode
   emacs-lisp-mode))

;; Add custom snippet directory
(setq yas-snippet-dirs (append yas-snippet-dirs '(".config/doom/snippets")))

;;; =========================== LANGUAGE CONFIGURATIONS ===========================
(load! "lang/c")
(load! "lang/org")
(load! "lang/lua")
(load! "lang/fish")
(load! "lang/rust")
(load! "lang/latex")
(load! "lang/julia")
(load! "lang/python")
(load! "lang/pkgbuild")

;; (setq-hook! 'web-mode-hook +format-with 'prettier-prettify)
(add-hook 'web-mode-hook 'prettier-js-mode)
;;; =============================== OTHER CONFIGS ==============================
;; (load! "project")
(load! "mapping")
(load! "util")
