;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================= Init procces things? ============================

;; Bug / Limitation of PGTK prevents S-SPC (shift space) from being detected, see:
;; https://www.reddit.com/r/emacs/comments/osscfd/pgtk_emacswaylandgnome_no_shiftspace/i4k9pxm/
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-07/msg00071.html
;; (setq pgtk-use-im-context-on-new-connection nil)
;; Check if frames are in use, otherwise this throws an error
;; (when window-system (pgtk-use-im-context nil))


;; fix for using fish shell, doom tells me to put this here
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))
;; ==================================== Basics ===================================

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Maya Pasricha"
      user-mail-address "shrey.pasricha@gmail.com")


;; =================================== Visuals ===================================

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

(setq
 default-font-size 30
 doom-font (font-spec :family "Iosevka Nerd Font"
                      :size default-font-size
                      :weight 'regular)
 doom-variable-pitch-font (font-spec :family "Iosevka Aile"
                                     :size default-font-size)
 doom-big-font (font-spec :family "Iosevka Nerd Font"
                          :size (+ 10 default-font-size)
                          :weight 'regular))

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

;; ============================== Tree-sitter config =============================
;; TODO: try using https://github.com/casouri/tree-sitter-module/issues/13 or https://codeberg.org/ckruse/treesit-parser-manager or emacs 29
;; TODO: Themeing?: https://emacs-tree-sitter.github.io/syntax-highlighting/customization/

(after! tree-sitter
  (add-to-list 'tree-sitter-load-path "/home/cosmicdoge/.cache/tree-sitter/lib/")
  (pushnew! tree-sitter-major-mode-language-alist '(web-mode . astro) '(glsl-mode . glsl) '(jsonc-mode . json5))
  ;; Use tree-sitter syntax highlighting in all supported modes.
  (setq +tree-sitter-hl-enabled-modes t))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(global-tree-sitter-mode)


;; ============================== LSP CONFIGURATION ==============================

(setq lsp-enable-symbol-highlighting nil)
(setq lsp-lens-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-sideline-show-code-actions t)


;; ============================== MISC CONFIGURATION =============================

(setq evil-snipe-spillover-scope 'whole-visible)

(setq ispell-dictionary "en_AU")
(setq ispell-personal-dictionary "~/.aspell.en_AU.pws")
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
(setq langtool-mother-tongue "en_AU")

;; Make PDFs (and SVGs?) look nicer (mayyybe)
;; (setq pdf-view-use-scaling t)
;; (pgtk-set-monitor-scale-factor "DP-2" 1.0)

;; aggressively indent in these modes
(use-package! aggressive-indent-mode
  :hook (org-mode
         lua-mode
         latex-mode
         fennel-mode
         python-mode
         emacs-lisp-mode))

;; Add custom snippet directory
(after! (yasnippet doom-snippets)
  (push ".dotfiles/doom/snippets" yas-snippet-dirs)
  (yas-reload-all)
  ;; add yas-snippet to hippie expand
  (push #'yas-hippie-try-expand hippie-expand-try-functions-list))

;; Git stuffs
(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (setq magit-diff-refine-hunk 'all))

;; =========================== LANGUAGE CONFIGURATIONS ===========================

;; Load all files from the (relative) lang/ dir
(mapc (lambda (f) (load! f))
      (directory-files (expand-file-name "lang" doom-user-dir) t "\\.el$"))

(use-package! markdown-xwidget
  :after markdown-mode
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "dark")
  (markdown-xwidget-mermaid-theme "dark")
  (markdown-xwidget-code-block-theme "agate"))

;; =============================== OTHER CONFIGS ==============================
(load! "mapping")
(load! "util")
