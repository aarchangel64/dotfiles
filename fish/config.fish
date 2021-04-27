# Starship prompt config (from https://github.com/starship/starship/discussions/2046#discussioncomment-274553)
# disable virtualenv prompt, it breaks starship
set VIRTUAL_ENV_DISABLE_PROMPT 1

function fish_mode_prompt; end
set -gx STARSHIP_SHELL "fish"

# Set up the session key that will be used to store logs
set -gx STARSHIP_SESSION_KEY (random 10000000000000 9999999999999999)


# starship init fish | source
source /etc/grc.fish
# set -x string split ' ' -f2 (alias) | string join ','
