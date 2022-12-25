;;; ================================== Navigation =================================

(map! :map evil-window-map
      ;; "SPC" #'rotate-layout
      ;; Navigation
      "<left>"          #'evil-window-left
      "<up>"            #'evil-window-up
      "<right>"         #'evil-window-right
      "<down>"          #'evil-window-down)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)


;; (map! :after nhexl-mode
;;       :map nhexl-mode-map
;;       :n "<left>"       #'nhexl-nibble-backward
;;       :n "<right>"      #'nhexl-nibble-forward)

;;; =============================== Utility Bindings ==============================

(map!
 :n "g C" #'banner-comment)

(map!
 :leader
 :desc "Tiny Expand" "i t" #'tiny-expand)

(map!
 :desc "Tiny Expand" :i "C-;" #'tiny-expand)
