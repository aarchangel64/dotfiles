# Starship prompt config (from https://github.com/starship/starship/discussions/2046#discussioncomment-274553)
# disable virtualenv prompt, it breaks starship
set VIRTUAL_ENV_DISABLE_PROMPT 1

function fish_mode_prompt; end
set -gx STARSHIP_SHELL "fish"

# Set up the session key that will be used to store logs
set -gx STARSHIP_SESSION_KEY (random 10000000000000 9999999999999999)


starship init fish | source
source /etc/grc.fish
# set -x string split ' ' -f2 (alias) | string join ','
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH

# pnpm
set -gx PNPM_HOME "/home/cosmicdoge/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH
# pnpm end

set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
