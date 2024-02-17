# Defined via `source`
function l --wraps='eza -Fl --group-directories-first' --wraps='eza --long --classify --git --header --group-directories-first' --description 'alias l=eza --long --classify --git --header --group-directories-first'
  eza --long --classify --git --header --group-directories-first $argv; 
end
