#!/usr/bin/fish

set theme_file ~/.dotfiles/xcolours/$argv
if test -e $theme_file
    cat $theme_file \
        | string replace '!' '#' \
        | string replace '*.' '' \
        | string replace ':' '' \
        >~/.dotfiles/kitty/theme.conf
else
    echo "Couldn't find $theme_file"
end
