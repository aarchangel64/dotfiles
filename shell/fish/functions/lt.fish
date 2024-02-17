# Defined via `source`
function lt --wraps='eza --group-directories-first -lFTL' --wraps='eza --long --classify --git --header --group-directories-first --tree --level' --description 'alias lt=eza --long --classify --git --header --group-directories-first --tree --level'
  set -q argv[1] || set argv[1] "2";
  eza --long --classify --git --header --group-directories-first --tree --level $argv; 
end
