function fish_greeting
    # 12.5% chance of showing pod the cat
    if test (random 1 8) -eq 1
        set pod_img ~/.dotfiles/resources/images/pod_cat.jpg
        set pos 11x11@1x1
    else
        set pod_img ~/.dotfiles/resources/images/pod.png
        set pos 7x7@2x0
    end

    kitty +kitten icat --align left --scale-up --place $pos $pod_img
    fortune ~/.dotfiles/resources/fortune/pod |
        fmt -w (math "round($COLUMNS * 0.75)") |
        boxes -d square-brackets -a c -p h2v1 |
        nl -bn -w 12
end
