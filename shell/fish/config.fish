# Rootless Docker
set -gx DOCKER_HOST unix://$XDG_RUNTIME_DIR/docker.sock

# bat-extras
# batman to replace man
export MANPAGER=env\ BATMAN_IS_BEING_MANPAGER=yes\ bash\ /usr/bin/batman
export MANROFFOPT=-c

# Starship prompt config (from https://github.com/starship/starship/discussions/2046#discussioncomment-274553)
# disable virtualenv prompt, it breaks starship
# set VIRTUAL_ENV_DISABLE_PROMPT 1

starship init fish | source
source /etc/grc.fish

# pnpm
set -gx PNPM_HOME "/home/cosmicdoge/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
