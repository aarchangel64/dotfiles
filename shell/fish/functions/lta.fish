# Defined via `source`
function lta --wraps='eza --group-directories-first -lFaTL' --wraps='eza --all --long --classify --git --header --group-directories-first --tree --level' --description 'alias lta=eza --all --long --classify --git --header --group-directories-first --tree --level'
  set -q argv[1] || set argv[1] "2";
  eza --all --long --classify --git --header --group-directories-first --tree --level $argv; 
end
