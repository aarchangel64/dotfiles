;;; ../../.dotfiles/doom/lang/lua.el -*- lexical-binding: t; -*-

;;; ==================================== Fennel ===================================

;; Based on https://www.reddit.com/r/emacs/comments/dx4lsy/question_about_counsel_compile/
(defun cosmic/counsel-compile-fennel ()
  (format "fennel --globals --add-package-path /usr/share/awesome/lib/*.lua --compile %1$s.fnl > %1$s.lua" (file-name-sans-extension (buffer-file-name))))

(use-package! fennel-mode
  :mode "\\.fnl\\'"
  ;; :bind (:map doom-leader-code-map
  ;;        ("c" . +default/compile))
  :config
  (setq compilation-read-command nil)
  (defun fennel-format ()
    "Run fnlmfmt on the current buffer."
    (interactive)
    (shell-command-on-region (point-min) (point-max) "fnlfmt --indent-width 2 -" nil t))
  )

(after! (mode-local fennel-mode)
  (setq-mode-local fennel-mode compile-command #'(cosmic/counsel-compile-fennel)))

(map! :mode fennel-mode
      :map doom-leader-code-map
      "c" #'+default/compile
      "f" #'fennel-format)


;;; ===================================== Lua =====================================

(after! lua-mode
  (setq lsp-lua-workspace-library "'Lua.workspace.library': {'/usr/share/awesome/lib': true}")
  (setq lsp-lua-diagnostics-globals "'Lua.diagnostics.globals': ['use', 'awesome', 'client', 'root']"))
