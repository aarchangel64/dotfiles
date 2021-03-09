(after! org
  (setq evil-org-movement-bindings '((up . "<up>") (down . "<down>") (left . "<left>") (right . "<right>")))
  (setq org-todo-keywords
        '((sequence "TODO(!t/)" "IN PROGRESS(!p/)" "FINISHING UP(f)" "|" "DONE(!d/)")))
  ;; (setq org-todo-keyword-faces
  ;;       '(("IN PROGRESS" . (:foreground "yellow")))
  ;; )
  )

(map! :map evil-window-map
      ;; "SPC" #'rotate-layout
      ;; Navigation
      "<left>"          #'evil-window-left
      "<up>"            #'evil-window-up
      "<right>"         #'evil-window-right
      "<down>"          #'evil-window-down
      )


(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward
      )

;; (map! :n "K" #'+lookup/documentation-new-window)
