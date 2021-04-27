function fish_greeting
    kitty +kitten icat --align left --scale-up --place 7x7@2x0 ~/.dotfiles/images/pod.png
    fortune ~/.dotfiles/fortune/pod |
        fmt -w (math "round($COLUMNS * 0.75)") |
        boxes -d stone -a c -s x5 |
        nl -bn -w 10
    # fortune -es -n 26 | toilet -f pagga -F border -t | lolcat -t -F .2
    # # echo "$hostname" | toilet -f pagga -F border -t | lolcat -t -F .2
    # # neofetch
    # __draw-divider
end
