;;; ../../.dotfiles/doom/lang/fish.el -*- lexical-binding: t; -*-

;;; ================================== Fish shell =================================

(add-hook! fish-mode
  (set-company-backend! 'fish-mode
    '(company-shell company-shell-env company-fish-shell company-files)))
