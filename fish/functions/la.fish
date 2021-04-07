# Defined via `source`
function la --wraps='exa -Fla --group-directories-first' --wraps='exa --all --long --classify --git --header --group-directories-first' --description 'alias la=exa --all --long --classify --git --header --group-directories-first'
  exa --all --long --classify --git --header --group-directories-first $argv; 
end
