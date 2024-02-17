# Defined via `source`
function la --wraps='eza -Fla --group-directories-first' --wraps='eza --all --long --classify --git --header --group-directories-first' --description 'alias la=eza --all --long --classify --git --header --group-directories-first'
  eza --all --long --classify --git --header --group-directories-first $argv; 
end
