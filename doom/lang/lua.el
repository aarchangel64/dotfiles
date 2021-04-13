;;; ../../.dotfiles/doom/lang/lua.el -*- lexical-binding: t; -*-

;;; ==================================== Fennel ===================================

;; Based on https://www.reddit.com/r/emacs/comments/dx4lsy/question_about_counsel_compile/

(defvar cosmic/fennel-path "")
(put 'cosmic/fennel-path 'safe-local-variable #'stringp)

(defun cosmic/counsel-compile-fennel ()
  (format "fennel --lua lua5.3 --globals awesome,screen --add-package-path ';%1$s?.lua;%2$s?.lua' --add-fennel-path ';%1$s?.fnl;%2$s?.fnl' --compile %3$s.fnl > %3$s.lua"
          (doom-project-root) cosmic/fennel-path (file-name-sans-extension (buffer-file-name))))

(defun fennel-format ()
  "Run fnlmfmt on the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "fnlfmt --indent-width 2 -" nil t))

(use-package! fennel-mode
  :mode "\\.fnl\\'"
  ;; :bind (:map doom-leader-code-map
  ;;        ("c" . +default/compile)
  ;;        ("f" . fennel-format))
  :config
  (setq compilation-read-command nil)
  )

(after! (mode-local fennel-mode)
  (setq-mode-local fennel-mode compile-command #'(cosmic/counsel-compile-fennel)))

(map! :mode fennel-mode
      :leader
      "c c" #'+default/compile
      "c f" #'fennel-format)


;;; ===================================== Lua =====================================

(after! lua-mode
  (setq lsp-lua-workspace-library "'Lua.workspace.library': {'/usr/share/awesome/lib': true}")
  (setq lsp-lua-diagnostics-globals "'Lua.diagnostics.globals': ['use', 'awesome', 'client', 'root']"))
