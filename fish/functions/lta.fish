# Defined via `source`
function lta --wraps='exa --group-directories-first -lFaTL' --wraps='exa --all --long --classify --git --header --group-directories-first --tree --level' --description 'alias lta=exa --all --long --classify --git --header --group-directories-first --tree --level'
  set -q argv[1] || set argv[1] "2";
  exa --all --long --classify --git --header --group-directories-first --tree --level $argv; 
end
