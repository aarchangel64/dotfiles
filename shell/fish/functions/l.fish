# Defined via `source`
function l --wraps='exa -Fl --group-directories-first' --wraps='exa --long --classify --git --header --group-directories-first' --description 'alias l=exa --long --classify --git --header --group-directories-first'
  exa --long --classify --git --header --group-directories-first $argv; 
end
