# function fish_prompt
#     switch "$fish_key_bindings"
#         case fish_hybrid_key_bindings fish_vi_key_bindings
#             set keymap "$fish_bind_mode"
#         case '*'
#             set keymap insert
#     end
#     set -l exit_code $status
#     # Account for changes in variable name between v2.7 and v3.0
#     set -l starship_duration "$CMD_DURATION$cmd_duration"
# 
#     # From https://github.com/starship/starship/discussions/2046#discussioncomment-274553
#     set --export STARSHIP_CONFIG "/home/cosmicdoge/.config/starship.toml"
# 
#     # "/home/cosmicdoge/.cargo/bin/starship" prompt --status=$exit_code --keymap=$keymap --cmd-duration=$starship_duration --jobs=(count (jobs -p))
# 
#     function decode
#         string replace --all --regex '\e(\[[\d;]*|\(B\e\[)m(\co)?' '' $argv
#     end
# 
#     set -l prompt ("/home/cosmicdoge/.cargo/bin/starship" prompt --status=$exit_code --keymap=$keymap --cmd-duration=$starship_duration --jobs=(count (jobs -p)))
#     set -l l_len (decode $prompt[2] | wc -L)
#     set -l r_len (decode $prompt[3] | wc -L)
#     set -l len (math $COLUMNS - $l_len - $r_len - 1)
#     set -l div (string repeat --no-newline -n $len '─')
#     set -l hfreq (math "0.15 * 80 / $len")
#     echo -en "\n$prompt[2]"
#     # set_color brgreen; echo -en "$div "; set_color normal;
#     echo -n "$div " | lolcat -b -f -h $hfreq
#     echo -en "$prompt[3]\n$prompt[4..-1]"
#     # echo $lprompt
# end
