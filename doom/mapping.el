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


;;; =============================== Utility Bindings ==============================

(map! :after banner-comment
      :n "g C" #'banner-comment)

