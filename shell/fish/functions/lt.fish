# Defined via `source`
function lt --wraps='exa --group-directories-first -lFTL' --wraps='exa --long --classify --git --header --group-directories-first --tree --level' --description 'alias lt=exa --long --classify --git --header --group-directories-first --tree --level'
  set -q argv[1] || set argv[1] "2";
  exa --long --classify --git --header --group-directories-first --tree --level $argv; 
end
