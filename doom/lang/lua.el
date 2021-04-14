;;; ../../.dotfiles/doom/lang/lua.el -*- lexical-binding: t; -*-

;;; ==================================== Fennel ===================================

;; Based on https://www.reddit.com/r/emacs/comments/dx4lsy/question_about_counsel_compile/

(defvar cosmic/fennel-path nil)
(defvar cosmic/package-path nil)
(defvar cosmic/fennel-args nil)
(put 'cosmic/fennel-path 'safe-local-variable #'stringp)
(put 'cosmic/package-path 'safe-local-variable #'stringp)
(put 'cosmic/fennel-args 'safe-local-variable #'stringp)
;; (put )

;; (setq enable-local-variables :all)
;; (setq enable-local-eval maybe)



(defun cosmic/counsel-compile-fennel ()
  (concat "fennel " cosmic/fennel-args " --add-package-path '"
          (mapconcat #'(lambda (x) (if x (format "%1$s?.lua;%1$s?/init.lua" x) "")) (list cosmic/package-path) ";")
          "' --add-fennel-path '"
          (mapconcat #'(lambda (x) (if x (format "%1$s?.fnl;%1$s?/init.fnl" x) "")) (list cosmic/fennel-path (doom-project-root)) ";")
          (format "' --compile %1$s.fnl > %1$s.lua" (file-name-sans-extension (buffer-file-name)))))

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

(map! :after fennel-mode
      :map fennel-mode-map
      :leader "c c" #'+default/compile
      :leader "c f" #'fennel-format)


;;; ===================================== Lua =====================================

(after! lua-mode
  (setq lsp-lua-workspace-library "'Lua.workspace.library': {'/usr/share/awesome/lib': true}")
  (setq lsp-lua-diagnostics-globals "'Lua.diagnostics.globals': ['use', 'awesome', 'client', 'root']"))
