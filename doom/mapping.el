(map! (:map evil-window-map
            ;; "SPC"             #'rotate-layout
            ;; Navigation
            "<left>"             #'evil-window-left
            "<up>"               #'evil-window-up
            "<right>"            #'evil-window-right
            "<down>"             #'evil-window-down)

      :n [mouse-8]               #'better-jumper-jump-backward
      :n [mouse-9]               #'better-jumper-jump-forward

      :i "C-;"                   #'tiny-expand
      :i "S-SPC"                 #'hippie-expand
      :n "g C"                   #'banner-comment

      (:leader
       :desc "Tiny Expand" "i t" #'tiny-expand)
      (:leader
       :desc "Generate Doc String" "c g" #'ts-docstr-at-point))
;; (map! :after nhexl-mode
;;       :map nhexl-mode-map
;;       :n "<left>"       #'nhexl-nibble-backward
;;       :n "<right>"      #'nhexl-nibble-forward)
